# WFMU explorer version 2.0
# ----------------- LOAD LIBRARIES ----------------------
# options for dev or for deployment
Sys.setenv(DUCKPLYR_FORCE = FALSE)
options(shiny.minified = TRUE)
options(shiny.autoreload = FALSE)
options("dplyr.summarise.inform" = FALSE)
options(duckdb.materialize_message = FALSE)
options(duckdb.progress_display = FALSE) # keep server logs readable

# ----------------- PERSISTENT CACHE ----------------------
# Derived artifacts (song search index, histogram bins, memoised query
# results) are persisted on disk so restarts don't redo the work. The cache
# directory name is keyed by the newest data-file mtime plus a code-version
# salt, so a rescrape (or bumping CACHE_VERSION after changing query
# semantics) automatically abandons the old cache. Location: $WFMU_CACHE_DIR
# if set, else the user cache dir (the app dir is root-owned on the server
# and the app runs as user `shiny`), else a session temp dir.
CACHE_VERSION <- "2.1"

resolve_cache_base <- function() {
  candidates <- c(
    Sys.getenv("WFMU_CACHE_DIR", unset = NA),
    tools::R_user_dir("wfmu_explorer", "cache"),
    file.path(tempdir(), "wfmu_explorer_cache")
  )
  for (d in candidates[!is.na(candidates) & nzchar(candidates)]) {
    ok <- dir.exists(d) || dir.create(d, recursive = TRUE, showWarnings = FALSE)
    if (ok && file.access(d, mode = 2) == 0) return(d)
  }
  stop("No writable cache directory found")
}

cache_base <- resolve_cache_base()
data_stamp <- format(
  max(file.mtime(list.files("data", full.names = TRUE))),
  "%Y%m%d%H%M%S"
)
cache_root <- file.path(cache_base, paste0("v", CACHE_VERSION, "_", data_stamp))
dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
# drop caches from earlier data/code versions
for (d in setdiff(list.files(cache_base, pattern = "^v", full.names = TRUE), cache_root)) {
  unlink(d, recursive = TRUE)
}
message("Cache dir: ", cache_root)

# persistent memoise cache under cache_root (results survive restarts)
disk_cache <- function(name, max_size, max_age = 24 * 3600) {
  cachem::cache_disk(
    dir = file.path(cache_root, paste0("memo_", name)),
    max_size = max_size,
    max_age = max_age
  )
}

# DuckDB extensions persist here instead of being re-downloaded per session
options(duckdb.extension_directory = file.path(cache_base, "duckdb_extensions"))

library(dplyr)
library(htmltools)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(memoise)
library(wordcloud2)
library(lubridate)
library(igraph)
library(circlize)
library(stringr)
library(ggplot2)
library(zoo)
library(ggthemes)
library(tm)
library(duckplyr)
library(DT)
methods_overwrite()

# Production host is a 2 GB t3.small shared by R, DuckDB and Shiny Server.
# DuckDB's default memory_limit is 80% of RAM, which would starve R; cap it
# and let large aggregates spill to disk instead of failing.
db_exec("SET memory_limit = '600MB'")
db_exec("SET threads = 2")
db_exec(paste0("SET temp_directory = '", file.path(tempdir(), "duckdb_spill"), "'"))

# set info to true for debugging
# fallback_config(info = FALSE, logging = FALSE)

load('data/djdtm.rdata') # document term object for similarity

# Only the 30 histogram bins and labels are needed from the 6 MB precomputed
# ggplot "gg_sim"; extract once and cache them so later starts skip the load().
sim_hist_file <- file.path(cache_root, "sim_hist_bins.rds")
if (file.exists(sim_hist_file)) {
  sim_hist <- readRDS(sim_hist_file)
} else {
  load('data/similarity_histogram_gg.rdata')
  sim_hist <- list(
    bars = ggplot2::layer_data(gg_sim, 1),
    labels = gg_sim$labels[c("x", "y", "title")]
  )
  saveRDS(sim_hist, sim_hist_file)
  rm(gg_sim)
}
playlists <- read_file_duckdb('data/playlists.parquet', "read_parquet")

djKey <- read_file_duckdb('data/djKey.parquet', "read_parquet") |>
  # preserve only unique DJs
  distinct(DJ, .keep_all = TRUE) |>
  arrange(ShowName) |>
  as_tibble()

djSimilarity <- read_file_duckdb(
  'data/djsimilarity.parquet',
  "read_parquet"
)

djDistinctive <- read_file_duckdb(
  'data/distinctive_artists.parquet',
  "read_parquet"
)

source("wordcloud2a.R")

# ----------------- DO SETUP ----------------------
HOST_URL <- "wfmu.artsteinmetz.com"
default_song <- "Help"
default_artist <- 'Abba'
default_artist_multi <- c('Abba', 'Beatles')
bot_shows <- c("NO", "RQ", "SR") # show with bot DJs
# plays before a channel existed are excluded when that channel is selected
# on the Station tab. Known channels are listed explicitly; any channel that
# later appears in djKey is added below with a start date of today - 90 days.
channel_start_dates <- tibble(
  channel = c("WFMU", "Archive", "Give the Drummer", "Rock & Soul", "Sheena's Jungle Room"),
  start_date = as.Date(c("1970-01-01", "1970-01-01", "2010-01-01", "2010-01-01", "2022-01-01"))
)
channel_start <- function(channel) {
  d <- channel_start_dates$start_date[channel_start_dates$channel == channel]
  if (length(d) == 0) as.Date("1970-01-01") else d[1]
}

max_date <- summarize(playlists, max(AirDate)) |> pull()
min_date <- summarize(playlists, min(AirDate)) |> pull()
max_year <- max(year(max_date))
min_year <- min(year(min_date))

# convert years range to date range
ytd <- function(years_range) {
  years_range <- c(
    as.Date(paste0(round(years_range[1]), "-1-1")),
    as.Date(paste0(round(years_range[2]), "-12-31"))
  )
  return(years_range)
}
#limit DJ list to DJs that are present in playlist file

djKey <- select(playlists, DJ) |>
  distinct() |>
  left_join(djKey, by = "DJ") |>
  # remove NA Channel DJs
  filter(!is.na(ShowName)) |>
  # materialize once; djKey is small and looked up in nearly every output
  collect()

# fast ShowName -> DJ code lookup, avoids a DuckDB round trip per lookup
show_to_dj <- setNames(djKey$DJ, djKey$ShowName)

channel_names <- unique(na.omit(djKey$Channel))

# reconcile channel_start_dates with the channels actually present: new
# channels get a recent start date; channels that have vanished are kept
new_channels <- setdiff(channel_names, channel_start_dates$channel)
if (length(new_channels) > 0) {
  message("New channel(s) in djKey, assuming start ", Sys.Date() - 90, ": ",
          paste(new_channels, collapse = ", "))
  channel_start_dates <- bind_rows(
    channel_start_dates,
    tibble(channel = new_channels, start_date = Sys.Date() - 90)
  )
}

all_artisttokens <- distinct(select(playlists, ArtistToken)) |> pull()

# ----------------- STATION TAB QUERIES ----------------------
# Defined at global scope so the memoise cache is shared across sessions.
# Inputs form a small discrete set (channel x 2 x 2 x year pairs) and each
# result is ~125 rows, so the cache stays tiny; bound it anyway.
station_dj_codes <- function(channel = "ALL", exclude_wake = FALSE, exclude_bots = TRUE) {
  codes <- if (channel == "ALL") djKey$DJ else djKey$DJ[djKey$Channel == channel]
  if (exclude_wake) codes <- setdiff(codes, "WA")
  if (exclude_bots) codes <- setdiff(codes, bot_shows)
  codes
}

get_station_stats <- memoise(
  function(
    channel = "ALL",
    exclude_wake = FALSE,
    exclude_bots = TRUE,
    years_range = c(2010, 2023),
    exclude_signature = TRUE
  ) {
    # duckplyr needs plain scalars in filter expressions (no `x[1]` indexing)
    yr <- ytd(years_range)
    y1 <- yr[1]
    y2 <- yr[2]
    # a channel's DJs may have earlier shows on other channels; don't count
    # plays from before the selected channel existed
    if (channel != "ALL") {
      y1 <- max(y1, channel_start(channel))
    }
    dj_codes <- station_dj_codes(channel, exclude_wake, exclude_bots)

    # one filtered base relation shared by both aggregates; semi_join because
    # duckplyr won't translate `%in%` against a long vector
    base <- playlists |>
      select(DJ, AirDate, ArtistToken, Title, Signature) |>
      filter(AirDate >= y1, AirDate <= y2) |>
      semi_join(tibble(DJ = dj_codes), by = "DJ")
    if (exclude_signature) base <- filter(base, !Signature)

    artists <- base |>
      filter(ArtistToken != "", ArtistToken != "Unknown") |>
      summarize(.by = ArtistToken, play_count = n()) |>
      arrange(desc(play_count)) |>
      head(100) |>
      collect()

    songs_agg <- base |>
      filter(Title != "", Title != "Unknown") |>
      summarize(.by = c(ArtistToken, Title), play_count = n())

    list(
      artists = artists,
      songs = songs_agg |> arrange(desc(play_count)) |> head(25) |> collect(),
      # number of distinct artist/title pairs (kept lazy; never collect all songs)
      count = songs_agg |> summarize(n = n()) |> pull(n)
    )
  },
  cache = disk_cache("station", max_size = 20 * 1024^2)
)

# prime the cache with the Station tab's default view so the first visitor
# doesn't wait for it
station_defaults <- list(
  channel = "ALL",
  exclude_wake = FALSE,
  exclude_bots = TRUE,
  years_range = c(max_year - 3, max_year),
  exclude_signature = TRUE
)
invisible(do.call(get_station_stats, station_defaults))

# ----------------- DJ TAB QUERIES ----------------------
# DJ Profile: one filtered base relation feeds both aggregates
get_dj_stats <- memoise(
  function(dj = "TW", years_range = c(2017, 2019), exclude_signature = TRUE) {
    yr <- ytd(years_range)
    y1 <- yr[1]
    y2 <- yr[2]
    base <- playlists |>
      select(DJ, AirDate, ArtistToken, Title, Signature) |>
      filter(DJ == dj, AirDate >= y1, AirDate <= y2)
    if (exclude_signature) base <- filter(base, !Signature)
    list(
      artists = base |>
        summarize(.by = ArtistToken, play_count = n()) |>
        arrange(desc(play_count)) |>
        head(100) |>
        collect(),
      songs = base |>
        summarize(.by = c(ArtistToken, Title), play_count = n()) |>
        arrange(desc(play_count)) |>
        head(25) |>
        collect()
    )
  },
  cache = disk_cache("dj_stats", max_size = 30 * 1024^2)
)

get_similar_DJs <- memoise(
  function(dj = "KF") {
    djSimilarity |>
      filter(DJ1 == dj) |>
      arrange(desc(Similarity)) |>
      head(10) |>
      rename(DJ = DJ2) |>
      select(DJ, Similarity) |>
      collect() |>
      # add target dj so the chord chart has its 2-letter code; self-similarity = 100%
      bind_rows(tibble(DJ = dj, Similarity = 1)) |>
      left_join(djKey, by = "DJ") |>
      arrange(desc(Similarity)) |>
      select(ShowName, DJ, Channel, showCount, Similarity) |>
      mutate(Similarity = Similarity * 100)
  },
  cache = disk_cache("similar_djs", max_size = 10 * 1024^2)
)

get_sim_index <- memoise(
  function(dj1 = "TW", dj2 = "CF") {
    djSimilarity |>
      filter(DJ1 == dj1, DJ2 == dj2) |>
      pull(Similarity)
  },
  cache = disk_cache("sim_index", max_size = 5 * 1024^2)
)

# Compare Two DJs: a single DuckDB pass over both DJs' plays, then derive the
# artists-in-common and songs-in-common tables in R. `agg` is transient
# (tens of thousands of rows); only the two 10-row results are cached.
compare_djs <- memoise(
  function(dj1 = "TW", dj2 = "CF", exclude_signature = TRUE) {
    base <- playlists |>
      filter(DJ %in% c(dj1, dj2))
    if (exclude_signature) base <- filter(base, !Signature)
    agg <- base |>
      summarise(.by = c(DJ, ArtistToken, Title), n = n()) |>
      collect()
    totals <- agg |> summarise(.by = DJ, total = sum(n))

    # share of each DJ's total plays; keep each DJ's top `top_n` keys, join on
    # the keys, rank by summed share
    in_common <- function(keys, top_n = Inf) {
      by_key <- agg |>
        summarise(.by = c(DJ, all_of(keys)), n = as.integer(sum(n))) |>
        left_join(totals, by = "DJ") |>
        mutate(f = n / total) |>
        select(-total)
      side <- function(dj, f_name) {
        by_key |>
          filter(DJ == dj) |>
          arrange(desc(f)) |>
          head(top_n) |>
          select(all_of(keys), !!dj := n, !!f_name := f)
      }
      inner_join(side(dj1, "f1"), side(dj2, "f2"), by = keys) |>
        mutate(sum_f = f1 + f2) |>
        arrange(desc(sum_f)) |>
        head(10) |>
        select(all_of(keys), all_of(c(dj1, dj2)))
    }
    list(
      # artists: both DJs' top 500, as in the original app
      artists = in_common("ArtistToken", top_n = 500),
      # songs: no cutoff -- two DJs rarely share top-500 artist/title pairs
      songs = in_common(c("ArtistToken", "Title"))
    )
  },
  cache = disk_cache("compare_djs", max_size = 10 * 1024^2)
)

# Lightweight version of the precomputed similarity histogram: keep the
# binned bars, drop the ~230k raw pair similarities so each render is cheap.
gg_sim_light <- local({
  bars <- sim_hist$bars
  # original maps y = after_stat(count) + 1 on a log10 scale
  # styled to match the app's other plots (solarized dark on black)
  ggplot(bars, aes(x = x, y = count + 1)) +
    geom_col(
      width = bars$xmax[1] - bars$xmin[1],
      fill = "#268bd2",
      # outline in the solarized-dark panel colour to separate adjacent bars
      colour = "#073642"
    ) +
    scale_y_log10(labels = function(x) format(x, scientific = FALSE, trim = TRUE)) +
    # reading aid in the empty upper-right of the panel (y is log scale)
    # ggplot2:: because tm/NLP masks annotate()
    ggplot2::annotate(
      "text",
      x = 0.57, y = 3e4, hjust = 1,
      label = "More Similar",
      colour = "#93a1a1", size = 5.2 # ~14.8 pt
    ) +
    ggplot2::annotate(
      "segment",
      x = 0.59, xend = 0.72, y = 3e4, yend = 3e4,
      colour = "#93a1a1", linewidth = 1,
      arrow = arrow(length = unit(0.25, "cm"), type = "closed")
    ) +
    labs(x = sim_hist$labels$x, y = sim_hist$labels$y, title = sim_hist$labels$title) +
    # base_size 14 = default 12 + 2 pt for all text elements
    theme_solarized_2(light = FALSE, base_size = 14) +
    theme(plot.background = element_rect(fill = "black"))
})

# ----------------- ARTIST TAB QUERIES ----------------------
# One DuckDB pass per (tokens, years), shared by Single and Multi Artist and
# collapsed to (AirDate, DJ, ArtistToken, Title, Signature) counts. Threshold,
# Wake 'n' Bake / signature-song exclusion and the quarterly/yearly rollups are
# then cheap R steps on the cached result, so toggling those controls never
# touches DuckDB.
get_artist_plays <- memoise(
  function(tokens, years_range) {
    yr <- ytd(years_range)
    y1 <- yr[1]
    y2 <- yr[2]
    base <- playlists |>
      filter(AirDate >= y1, AirDate <= y2)
    # duckplyr translates %in% only up to 100 values
    if (length(tokens) <= 100) {
      base <- filter(base, ArtistToken %in% tokens)
    } else {
      base <- semi_join(base, tibble(ArtistToken = tokens), by = "ArtistToken")
    }
    base |>
      summarise(.by = c(AirDate, DJ, ArtistToken, Title, Signature), n = n()) |>
      collect()
  },
  cache = disk_cache("artist_plays", max_size = 50 * 1024^2)
)

get_artist_variants <- memoise(
  function(tokens) {
    playlists |>
      filter(ArtistToken %in% tokens) |>
      distinct(Artist) |>
      arrange(Artist) |>
      collect()
  },
  cache = disk_cache("artist_variants", max_size = 10 * 1024^2)
)

# Pure-R rollups of get_artist_plays() output (plain tibbles, no DuckDB).
# Date conversions are done with base R assignment so duckplyr never sees
# as.yearqtr()/year() and no methods_restore() toggling is needed.
drop_plays <- function(plays, exclude_wake = FALSE, exclude_signature = TRUE) {
  if (exclude_wake) plays <- plays[plays$DJ != "WA", ]
  if (exclude_signature) plays <- plays[!plays$Signature, ]
  plays
}

quarterly_by_dj <- function(plays, threshold = 3, exclude_wake = FALSE,
                            exclude_signature = TRUE) {
  plays <- drop_plays(plays, exclude_wake, exclude_signature)
  plays$AirDate <- as.yearqtr(plays$AirDate)
  plays |>
    summarise(.by = c(AirDate, DJ), Spins = as.integer(sum(n))) |>
    mutate(DJ = if_else(Spins < threshold, "AllOther", DJ)) |>
    summarise(.by = c(AirDate, DJ), Spins = as.integer(sum(Spins))) |>
    left_join(select(djKey, DJ, ShowName), by = "DJ") |>
    mutate(ShowName = if_else(is.na(ShowName), "AllOther", ShowName)) |>
    select(AirDate, Spins, ShowName) |>
    arrange(AirDate)
}

artist_top_songs <- function(plays, exclude_wake = FALSE, exclude_signature = TRUE) {
  drop_plays(plays, exclude_wake, exclude_signature) |>
    summarise(.by = Title, count = as.integer(sum(n))) |>
    arrange(desc(count))
}

artist_yearly <- function(plays, exclude_wake = FALSE, exclude_signature = TRUE) {
  plays <- drop_plays(plays, exclude_wake, exclude_signature)
  plays$AirDate <- year(plays$AirDate)
  plays |>
    summarise(.by = c(AirDate, ArtistToken), Spins = as.integer(sum(n))) |>
    arrange(AirDate)
}

# ----------------- SONG TAB QUERIES ----------------------
# Type-ahead search index: one row per distinct title with its play count.
# Lives in DuckDB memory (~26 MB), not in R. Building it from the playlist
# file takes ~1 s, so the result is cached as parquet and reloaded from there.
song_index_file <- file.path(cache_root, "song_index.parquet")
if (!file.exists(song_index_file)) {
  db_exec(sprintf(
    "COPY (
       SELECT Title, lower(Title) AS title_lc, count(*)::INTEGER AS n
       FROM read_parquet('data/playlists.parquet')
       WHERE Title <> '' AND Title <> 'Unknown'
       GROUP BY Title
     ) TO '%s' (FORMAT PARQUET)",
    gsub("\\\\", "/", song_index_file)
  ))
}
db_exec(sprintf(
  "CREATE OR REPLACE TABLE song_index AS SELECT * FROM read_parquet('%s')",
  gsub("\\\\", "/", song_index_file)
))

song_search_min_chars <- 2

# case-insensitive "contains" search, most-played first
search_songs <- function(q, limit = 50) {
  q <- tolower(trimws(q))
  if (nchar(q) < song_search_min_chars) {
    return(tibble(Title = character(), n = integer()))
  }
  q_sql <- gsub("'", "''", q, fixed = TRUE)
  read_sql_duckdb(sprintf(
    "SELECT Title, n FROM song_index
     WHERE contains(title_lc, '%s')
     ORDER BY n DESC, Title
     LIMIT %d",
    q_sql,
    as.integer(limit)
  )) |>
    collect()
}

song_index_lookup <- function(titles) {
  in_list <- paste0("'", gsub("'", "''", titles, fixed = TRUE), "'", collapse = ", ")
  read_sql_duckdb(sprintf("SELECT Title, n FROM song_index WHERE Title IN (%s)", in_list)) |>
    collect()
}

# selectize option rows. Shiny's selectize wrapper uses valueField "value",
# labelField "label" and searchField "label" (not selectize's own defaults),
# so server-loaded options must carry a `label` field.
song_choices <- function(res) {
  data.frame(
    value = res$Title,
    label = sprintf("%s  (%s plays)", res$Title, format(res$n, big.mark = ",", trim = TRUE)),
    stringsAsFactors = FALSE
  )
}

# named vector for a static selectizeInput(): names are labels, values are titles
song_choices_named <- function(titles) {
  ch <- song_choices(song_index_lookup(titles))
  setNames(ch$value, ch$label)
}
default_song_choices <- song_choices_named(default_song)

# shared by the UI and by updateSelectizeInput(), whose config *replaces*
# the UI's options rather than merging with them
song_selectize_options <- list(
  valueField = "value",
  labelField = "label",
  searchField = "label",
  placeholder = "start typing a song title...",
  loadThrottle = 300,
  maxOptions = 50,
  closeAfterSelect = TRUE
)

# One DuckDB pass per (titles, years), shared by the plot and the artist table
get_song_plays <- memoise(
  function(titles, years_range) {
    yr <- ytd(years_range)
    y1 <- yr[1]
    y2 <- yr[2]
    base <- playlists |>
      filter(AirDate >= y1, AirDate <= y2)
    if (length(titles) <= 100) {
      base <- filter(base, Title %in% titles)
    } else {
      base <- semi_join(base, tibble(Title = titles), by = "Title")
    }
    base |>
      summarise(.by = c(AirDate, DJ, ArtistToken, Signature), n = n()) |>
      collect()
  },
  cache = disk_cache("song_plays", max_size = 50 * 1024^2)
)

song_top_artists <- function(plays, exclude_wake = FALSE, exclude_signature = TRUE) {
  drop_plays(plays, exclude_wake, exclude_signature) |>
    summarise(.by = ArtistToken, count = as.integer(sum(n))) |>
    arrange(desc(count))
}

# ----------------- PLAYLIST TAB QUERY ----------------------
# Dedicated SQL so rows can be ordered by the parquet row number, which
# reflects play order within a show (the global `playlists` object is
# untouched). `dj` is a 2-letter code from djKey; dates are Date objects.
get_playlist <- memoise(
  function(dj, d1, d2) {
    read_sql_duckdb(sprintf(
      "SELECT AirDate, Artist, Title, Signature
       FROM read_parquet('data/playlists.parquet', file_row_number = true)
       WHERE DJ = '%s' AND AirDate BETWEEN DATE '%s' AND DATE '%s'
       ORDER BY AirDate, file_row_number",
      gsub("'", "''", dj, fixed = TRUE),
      format(as.Date(d1)),
      format(as.Date(d2))
    )) |>
      collect()
  },
  # full histories can be tens of MB; keep only a few around, briefly
  cache = disk_cache("playlist", max_size = 40 * 1024^2, max_age = 6 * 3600)
)

default_show <- "Ken"
default_show_last <- djKey$LastShow[djKey$ShowName == default_show][1]

#  DEFINE USER INTERFACE ===============================================================
# Built by a function so it can be regenerated: bslib assigns each tabset a
# random 4-digit id and two tabsets occasionally collide, which makes one
# tabset's links activate the other's panes (e.g. Artists menu toggling the
# Station word cloud/table). See ui <- build_unique_ui() below.
build_ui <- function() {
  navbarPage(
    "WFMU Playlist Explorer",
    theme = shinytheme("darkly"),
    # -- Add Tracking JS File
    #rest of UI doesn't initiate unless tab is clicked on if the code below runs
    #tags$head(includeScript("google-analytics.js"))
    header = tags$head(includeHTML(("google-analytics.html"))),
    # --------- Station TAB ----------------------------------
    tabPanel(
      "Station",
      titlePanel(HTML(paste0(
        "Top Artists and Songs Played on ",
        a("WFMU", href = "https://wfmu.org")
      ))),
      textOutput("date_span"),
      fluidPage(
        # ---- Sidebar layout with input and output definitions ----
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "channel",
              "Channel:",
              choices = c('ALL', channel_names),
              selectize = TRUE,
              selected = "ALL"
            ),
            checkboxInput(
              "exclude_wake",
              "Exclude Wake 'n' Bake?",
              value = FALSE
            ),
            checkboxInput(
              "exclude_bots",
              "Exclude Robot DJs?",
              value = TRUE
            ),
            checkboxInput(
              "exclude_signature",
              "Exclude Signature Songs?",
              value = TRUE
            ),
            helpText(
              'Be aware a wide date range could take many seconds to process.'
            ),
            sliderInput(
              "years_range_1",
              "Year Range:",
              min = min_year,
              max = max_year,
              sep = "",
              step = 1,
              round = TRUE,
              value = c(max_year - 3, max_year)
            ),
            textOutput("play_count"),
            h2(),
            actionButton("update", "Update View"),
            hr(),
            helpText(
              "NOTE: The Channel Selector filters for DJs currently on that channel ",
              "and only counts plays from after that channel started. ",
              "Within that window it includes all the DJ's shows, even if their show ",
              "used to be on a different channel."
            )
          ),
          # ---------- Main panel for displaying outputs ----
          mainPanel(
            h4("Top Artists"),
            tabsetPanel(
              type = "tabs",
              tabPanel("Word Cloud", withSpinner(wordcloud2Output("cloud"))),
              tabPanel("Table", tableOutput("table_artists"))
            ),
            h4("Songs"),
            tableOutput("table_songs")
          )
        )
      )
    ),
    # *-------- DJ TAB ----------------------------------
    navbarMenu(
      "DJs",
      # --------- DJs/DJ Profile -----------------------------
      tabPanel(
        "DJ Profile",
        titlePanel("DJ Profile"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "show_selection",
              "Show Name:",
              choices = sort(djKey$ShowName),
              selected = "Ken"
            ),
            textOutput("dj_current_channel"),
            hr(),
            # diplay a clickable url
            uiOutput("dj_profile_link"),
            hr(),
            htmlOutput("other_show_names"),
            hr(),
            uiOutput("DJ_date_slider"),
            checkboxInput(
              "exclude_signature_dj",
              "Exclude Signature Songs?",
              value = TRUE
            ),
            #, actionButton("DJ_update","Update")
            hr(),
            h4('Distinctive Artists'),
            h4(
              'Artists that are played relatively more by this DJ than other DJs'
            ),
            tableOutput("DJ_table_distinct_artists"),
            hr(),
            h5(
              "If you're curious, this is a term frequency - inverse document frequency (TF-IDF) analysis."
            )
          ),

          # Show Word Cloud
          mainPanel(
            fluidRow(
              h4('Top Artists'),
              tabsetPanel(
                type = "tabs",
                tabPanel(
                  "Word Cloud",
                  withSpinner(wordcloud2Output("DJ_cloud"))
                ),
                tabPanel("Table", tableOutput("DJ_table_artists"))
              )
            ),
            fluidRow(
              h4('Top Songs'),
              tableOutput("DJ_table_songs")
            )
          )
        )
      ),
      # --------- DJs/Find Simlar DJs -------------------
      tabPanel(
        "Find Similar DJs",
        titlePanel("Find Similar DJs"),
        sidebarLayout(
          # Sidebar with a slider and selection inputs
          sidebarPanel(
            selectInput(
              "show_selection_2",
              "Show Name:",
              choices = sort(djKey$ShowName),
              selected = "Ken"
            ),
            hr(),
            helpText(
              "NOTE: Channel is the DJ's current channel. ShowCount is all ",
              "their shows from any channel they have appeared on."
            )
          ),

          # Show Word Cloud
          mainPanel(
            fluidRow(
              h4('DJ Neighborhood'),
              withSpinner(plotOutput("DJ_chord"))
            ),
            fluidRow(
              h4('Most Similar Shows Based on Common Artists'),
              tableOutput("DJ_table_similar")
            )
          )
        )
      ),
      # --------- DJs/Compare Two DJs -----------------------
      tabPanel(
        "Compare Two DJs",
        titlePanel("Compare Two DJs"),
        fluidRow(
          column(
            4,
            selectInput(
              "show_selection_1DJ",
              "Show Name:",
              choices = sort(djKey$ShowName),
              selected = "Ken"
            )
          ),
          column(
            4,
            selectInput(
              "show_selection_4",
              "Show Name:",
              choices = sort(djKey$ShowName),
              selected = 'Bob Brainen'
            )
          ),
          column(
            4,
            # spacer so the checkbox lines up with the select boxes
            br(),
            checkboxInput(
              "exclude_signature_compare",
              "Exclude Signature Songs?",
              value = TRUE
            )
          )
        ),
        fluidRow(
          column(
            4,
            h4(
              'Similarity Index: ',
              textOutput("DJ_sim_index_text", inline = TRUE)
            ),
            h5(
              'Bars are the  frequency of all DJ pair similarities. Vertical line is similarity of this pair.'
            ),
            h5(
              ' The bulge at the low end shows WFMU DJs are not very similar to each other, in general.'
            )
          ),
          column(
            8,
            withSpinner(plotOutput("DJ_plot_sim_index", height = "200px"))
          )
        ),
        fluidRow(column(
          11,
          offset = 1,
          h4("Play Counts of Common Artists and Songs")
        )),
        fluidRow(
          column(
            5,
            h4('Artists in Common'),
            tableOutput("DJ_table_common_artists")
          ),
          column(7, h4('Songs in Common'), tableOutput("DJ_table_common_songs"))
        )
      )
    ), # end DJ tab
    # *-------- ARTISTS TAB ----------------------------------
    navbarMenu(
      "Artists",
      #----------- Single Artist -----------------------
      tabPanel(
        "Single Artist",
        titlePanel("Artists Plays by DJ Over Time"),
        sidebarLayout(
          # Sidebar with a slider and selection inputs
          sidebarPanel(
            fluidRow(
              h4('Artist names reduced to token of first two words.'),
              h4("Select one or more artists"),
              checkboxInput(
                "exclude_wake_artists",
                "Exclude Wake 'n' Bake?",
                value = FALSE
              ),
              checkboxInput(
                "exclude_signature_artists",
                "Exclude Signature Songs?",
                value = TRUE
              ),
              selectizeInput(
                "artist_selection_1DJ",
                label = NULL,
                choices = NULL,
                multiple = TRUE
              ),
              h4('Change the date range to include?'),
              sliderInput(
                "artist_years_range_1DJ",
                "Year Range:",
                min = min_year,
                max = max_year,
                sep = "",
                step = 1,
                round = TRUE,
                value = c(2002, max_year)
              ),
              h4('Change threshold to show DJ name?'),
              selectInput(
                "artist_all_other_1DJ",
                "Threshold of Minimum Plays to show DJ",
                selected = 3,
                choices = 1:9
              ),
              h4('Full artist names included in this token:'),
              tableOutput("artist_variants")
            )
          ),
          mainPanel(
            fluidRow(
              h4('Artist Plays per Quarter'),
              textOutput("chosen"),
              withSpinner(plotOutput("artist_history_plot_1DJ")),
              h4('Songs Played of this Artist'),
              tableOutput('top_songs_for_artist_1DJ')
            )
          )
        )
      ),
      # ---------- multi Artist ---------------
      tabPanel(
        "Multi Artist",
        titlePanel("Multi-Artist Plays Over Time"),
        sidebarLayout(
          # Sidebar with a slider and selection inputs
          sidebarPanel(
            fluidRow(
              h4('Artist names reduced to token of first two words.'),
              checkboxInput(
                "exclude_wake_artists_multi",
                "Exclude Wake 'n' Bake?",
                value = FALSE
              ),
              checkboxInput(
                "exclude_signature_artists_multi",
                "Exclude Signature Songs?",
                value = TRUE
              ),
              selectizeInput(
                "artist_selection_multi",
                h4("Select two or more artists"),
                choices = NULL,
                multiple = TRUE,
                #selected=default_artist_multi,
                options = list(closeAfterSelect = TRUE)
                #options = list(placeholder = 'select artist(s)')
              ),
              h4('Change the date range to include?'),
              sliderInput(
                "artist_years_range_multi",
                "Year Range:",
                min = min_year,
                max = max_year,
                sep = "",
                step = 1,
                round = TRUE,
                value = c(2002, max_year)
              ),
              h4('Full artist names included in these tokens:'),
              tableOutput("artist_variants_multi")
            )
          ),

          mainPanel(
            fluidRow(
              h4("Artist Plays Per Year."),
              withSpinner(plotOutput(
                "multi_artist_history_plot_4",
                width = "710px",
                height = "355px"
              )),
              h4('Artist Plays per Year (another way)'),
              plotOutput(
                "multi_artist_history_plot_2",
                width = "710px",
                height = "355px"
              ),
              h4('Artist Plays per Year (light version)'),
              h4('(The way Ken Likes to see it for WFMU site).'),
              plotOutput(
                "multi_artist_history_plot",
                width = "710px",
                height = "355px"
              ),
              h4()
            )
          )
        )
      )
    ),
    # *-------- SONGS TAB ----------------------------------
    tabPanel(
      "Songs",
      titlePanel("Find Songs"),
      sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
          h4('1) Choose the song(s).'),
          h5('Type part of a title; matches appear most-played first.'),
          h5('You can select more than one.'),
          selectizeInput(
            "song_selection",
            label = NULL,
            choices = default_song_choices,
            selected = default_song,
            multiple = TRUE,
            options = song_selectize_options
          ),
          h4('2) Change the date range?'),
          sliderInput(
            "song_years_range",
            "Year Range:",
            min = min_year,
            max = max_year,
            sep = "",
            value = c(2002, max_year)
          ),
          fluidRow(
            h4('3) Change threshold to show DJ?'),
            selectInput(
              "song_all_other",
              "Threshold of Minimum Plays to show DJ",
              selected = 3,
              choices = 1:9
            ),
            checkboxInput(
              "exclude_wake_songs",
              "Exclude Wake 'n' Bake?",
              value = FALSE
            ),
            checkboxInput(
              "exclude_signature_songs",
              "Exclude Signature Songs?",
              value = TRUE
            )
          )
        ),

        mainPanel(
          fluidRow(
            h4('Song Plays per Quarter'),
            withSpinner(plotOutput("song_history_plot")),
            h4('Most Played Artists for this Song'),
            tableOutput('top_artists_for_song')
          )
        )
      )
    ),
    # *--------- playlists tab --------------------
    tabPanel(
      "Playlists",
      titlePanel("Get a Playlist"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            h4("Choose a Show:"),
            selectInput(
              "show_selection_5",
              "Show Name:",
              choices = sort(djKey$ShowName),
              selected = "Ken"
            ),
            uiOutput("dj_playlist_link"),
            hr(),
            h4('Choose Date Range:'), #just to make some space for calendar
            dateRangeInput(
              "playlist_date_range",
              "Date Range:",
              # the selected DJ's most recent month; updated on show change
              start = default_show_last - 30,
              end = default_show_last,
              min = min_date,
              max = max_date
            ),
            actionButton(
              "reset_playlist_date_range",
              "Reset Dates to Full History"
            ),
            h4("Signature Songs:"),
            h5(
              "Songs a DJ plays nearly every show (show openers, closers, ",
              "theme songs) are flagged as signature songs. They are ",
              "highlighted and marked with a \u2605 in the playlist."
            ),
            h5(
              "Because they distort popularity measures, the other tabs ",
              "exclude them by default. Untick \"Exclude Signature Songs?\" ",
              "on those tabs to include them."
            )
          )
        ),

        # Show playlists
        mainPanel(
          fluidRow(
            h4("Playlist(s)"),
            withSpinner(DT::dataTableOutput("playlist_table")),
            h4()
          )
        )
      )
    ),
    # * -------- About ----------------------------------
    tabPanel(
      "About",
      mainPanel(
        includeMarkdown("about.md")
      )
    )
  ) # end UI
}

# pane element ids ("tab-<tabset>-<n>") must be unique in the document; a
# duplicate means two tabsets drew the same random id
tab_pane_ids <- function(ui) {
  html <- as.character(ui)
  regmatches(html, gregexpr('(?<=id=")tab-[0-9]+-[0-9]+', html, perl = TRUE))[[1]]
}

build_unique_ui <- function(max_tries = 20) {
  for (i in seq_len(max_tries)) {
    ui <- build_ui()
    if (!anyDuplicated(tab_pane_ids(ui))) {
      return(ui)
    }
  }
  stop("Could not build a UI with unique tabset ids")
}

ui <- build_unique_ui()
# DEFINE SERVER ===============================================================
server <- function(input, output, session) {
  # QUERY FUNCTIONS --------------------------------------------------------------
  # -------------- STATION TAB -----------------------------
  # Query functions live at global scope (get_station_stats) so their memoise
  # cache is shared across sessions. eventReactive() depends on input$update
  # (the action button) so the view only refreshes when the user clicks it.
  station_reactive <- eventReactive(
    input$update,
    {
      withProgress(message = "Processing...", {
        get_station_stats(
          input$channel,
          input$exclude_wake,
          input$exclude_bots,
          input$years_range_1,
          exclude_signature = input$exclude_signature
        )
      })
    },
    ignoreNULL = FALSE
  )

  # -------------- DJS TAB -----------------------------
  # Query functions (get_dj_stats, get_similar_DJs, get_sim_index, compare_djs)
  # live at global scope so their memoise caches are shared across sessions.

  # ---------------ARTIST TAB -----------------------------
  # Query functions (get_artist_plays, get_artist_variants) and the R rollups
  # (quarterly_by_dj, artist_top_songs, artist_yearly) are global.

  # ---------------SONG TAB -----------------------------
  # Query functions (search_songs, get_song_plays, song_top_artists) and the
  # song_index table are global.

  # ------------------ playlists tab --------------
  # get_playlist() is global (shared, bounded cache).

  # OUTPUT SECTON --------------------------------------------------------------
  # ------------------- station tab ----------------
  output$cloud <- renderWordcloud2({
    wordcloud2a(
      station_reactive()$artists,
      size = 0.3,
      backgroundColor = "black",
      color = 'random-light',
      ellipticity = 1
    )
  })

  output$table_artists <- renderTable({
    head(station_reactive()$artists, 25)
  })
  output$table_songs <- renderTable({
    station_reactive()$songs
  })
  output$play_count <- renderText({
    paste("Songs Played: ", format(station_reactive()$count, big.mark = ","))
  })
  output$date_span <- renderText({
    paste("Updated through", max_date)
  })
  # ------------------- DJs tab --------------------
  # the djKey row for the selected show; every Profile output reads from this
  dj_profile <- reactive({
    djKey[djKey$ShowName == input$show_selection, ][1, ]
  })

  # Slider value clamped to the selected DJ's active years. The slider is
  # re-rendered when the show changes, so its value is briefly stale; clamping
  # makes the stale run identical to the fresh one (a memoise hit) whenever the
  # slider was at full range.
  dj_years <- reactive({
    dj <- dj_profile()
    lo <- year(dj$FirstShow)
    hi <- year(dj$LastShow)
    yr <- input$DJ_years_range
    if (is.null(yr)) {
      c(lo, hi)
    } else {
      c(max(lo, round(yr[1])), min(hi, round(yr[2])))
    }
  })

  dj_stats <- reactive({
    withProgress(message = "Processing...", {
      get_dj_stats(
        dj_profile()$DJ,
        dj_years(),
        exclude_signature = input$exclude_signature_dj
      )
    })
  })

  output$dj_current_channel <- renderText({
    paste("Current Channel:", dj_profile()$Channel)
  })

  output$dj_profile_link <- renderUI({
    url <- a(
      " DJ's Home Page ",
      href = dj_profile()$profileURL,
      target = "_blank",
      rel = "noopener noreferrer",
      style = "
             border-radius: 25px;
             padding: 5px;
             background-color: teal;
             color: white"
    )
    tagList(url)
  })
  output$other_show_names <- renderUI({
    other_shows <- dj_profile()$other_shownames |>
      str_replace_all("\\n", "<br>")
    HTML(paste0("<h4>Other shows from this DJ:<h5><br>", other_shows))
  })

  output$DJ_date_slider <- renderUI({
    dj <- dj_profile()
    sliderInput(
      "DJ_years_range",
      "Year Range:",
      max = year(dj$LastShow),
      min = year(dj$FirstShow),
      sep = "",
      step = 1,
      round = TRUE,
      value = c(year(dj$FirstShow), year(dj$LastShow))
    )
  })

  output$DJ_cloud <- renderWordcloud2({
    wordcloud2a(
      dj_stats()$artists,
      size = 0.3,
      backgroundColor = "black",
      color = 'random-light',
      ellipticity = 1
    )
  })
  output$DJ_table_distinct_artists <- renderTable({
    dj1 <- dj_profile()$DJ
    djDistinctive |>
      filter(DJ == dj1) |>
      select(-DJ) |>
      head(25) |>
      collect()
  })
  output$DJ_table_artists <- renderTable({
    dj_stats()$artists
  })
  output$DJ_table_songs <- renderTable({
    dj_stats()$songs
  })

  output$DJ_table_similar <- renderTable({
    get_similar_DJs(show_to_dj[[input$show_selection_2]])
  })
  output$DJ_chord <- renderPlot(
    {
      dj1 <- show_to_dj[[input$show_selection_2]]
      # get similar djs but remove target dj or matrix stuff will break
      sim_DJs <- get_similar_DJs(dj1) |> filter(DJ != dj1) |> pull(DJ)
      dj_mat <- dj_mat <- as.matrix(djdtm[c(sim_DJs, dj1), ])
      adj_mat1 = dj_mat %*% t(dj_mat)
      # set zeros in diagonal
      diag(adj_mat1) = 0
      #change from dJ to show name
      #dimnames(adj_mat1)<-rep(list(Docs=filter(djKey,DJ  %in% row.names(adj_mat1)) |> pull(ShowName)),2)
      # create graph from adjacency matrix
      graph_artists1 = graph_from_adjacency_matrix(
        adj_mat1,
        mode = "undirected",
        weighted = TRUE,
        diag = FALSE
      )
      # get edgelist 1
      edges1 = as_edgelist(graph_artists1)

      # arc widths based on graph_artists1
      w1 = E(graph_artists1)$weight
      lwds = w1 / 20000
      #chord diagrams
      cdf <- bind_cols(as_tibble(edges1, .name_repair = "unique"), value = lwds)
      colset <- RColorBrewer::brewer.pal(11, 'Paired')
      par(mar = rep(0, 4), bg = "black", fg = "white")
      chordDiagram(cdf, annotationTrack = c('grid', 'name'), grid.col = colset)
      text(1, 1, labels = HOST_URL)
    },
    bg = "black"
  )

  # Compare Two DJs
  compare_pair <- reactive({
    c(show_to_dj[[input$show_selection_1DJ]], show_to_dj[[input$show_selection_4]])
  })
  compare_stats <- reactive({
    withProgress(message = "Processing...", {
      p <- compare_pair()
      compare_djs(p[1], p[2], exclude_signature = input$exclude_signature_compare)
    })
  })

  output$DJ_sim_index_text <- renderText({
    p <- compare_pair()
    paste(round(get_sim_index(p[1], p[2]) * 100), "%")
  })

  output$DJ_plot_sim_index <- renderPlot(
    {
      p <- compare_pair()
      gg_sim_light +
        geom_vline(
          xintercept = get_sim_index(p[1], p[2]),
          color = 'yellow',
          linewidth = 2
        )
    },
    bg = "black"
  )

  output$DJ_table_common_songs <- renderTable({
    compare_stats()$songs
  })
  output$DJ_table_common_artists <- renderTable({
    compare_stats()$artists
  })

  # ------------------- artist tab -------------------------------------------
  #---------------------- single artist tab-----------------------------------

  updateSelectizeInput(
    session = session,
    inputId = "artist_selection_1DJ",
    choices = all_artisttokens,
    server = TRUE,
    selected = default_artist
  )

  # one DuckDB pass per (artists, years); threshold / wake toggles are R-only
  artist_plays_1DJ <- reactive({
    req(input$artist_selection_1DJ)
    withProgress(message = "Processing...", {
      get_artist_plays(input$artist_selection_1DJ, input$artist_years_range_1DJ)
    })
  })

  output$artist_history_plot_1DJ <- renderPlot(
    {
      artist_history <- quarterly_by_dj(
        artist_plays_1DJ(),
        threshold = as.numeric(input$artist_all_other_1DJ),
        exclude_wake = input$exclude_wake_artists,
        exclude_signature = input$exclude_signature_artists
      )

      gg <- artist_history |>
        ggplot(aes(AirDate, Spins, fill = ShowName)) +
        geom_col(orientation = "x") +
        scale_x_yearqtr(format = "%Y", guide = guide_axis(check.overlap = TRUE))
      gg <- gg +
        labs(
          title = paste(
            "Number of",
            input$artist_selection_1DJ,
            "plays every quarter by DJ"
          ),
          x = "Date",
          caption = HOST_URL
        )
      gg <- gg +
        theme_solarized_2(light = FALSE) +
        scale_colour_solarized("red")
      gg <- gg + theme(plot.background = element_rect(fill = "black"))
      gg
    },
    bg = "black"
  )
  output$top_songs_for_artist_1DJ <- renderTable({
    artist_top_songs(
      artist_plays_1DJ(),
      input$exclude_wake_artists,
      input$exclude_signature_artists
    )
  })
  output$artist_variants <- renderTable({
    req(input$artist_selection_1DJ)
    get_artist_variants(input$artist_selection_1DJ)
  })
  #---------------------- multi artist tab -----------------------
  updateSelectizeInput(
    session = session,
    inputId = "artist_selection_multi",
    choices = all_artisttokens,
    server = TRUE,
    selected = default_artist_multi
  )

  artist_plays_multi <- reactive({
    req(input$artist_selection_multi)
    withProgress(message = "Processing...", {
      get_artist_plays(input$artist_selection_multi, input$artist_years_range_multi)
    })
  })
  reactive_multi_artists <- reactive({
    artist_yearly(
      artist_plays_multi(),
      input$exclude_wake_artists_multi,
      input$exclude_signature_artists_multi
    )
  })

  output$artist_variants_multi <- renderTable({
    req(input$artist_selection_multi)
    get_artist_variants(input$artist_selection_multi)
  })

  output$multi_artist_history_plot <- renderPlot(
    {
      multi_artist_history <- reactive_multi_artists()
      gg <- multi_artist_history |>
        ggplot(aes(x = AirDate, y = Spins, fill = ArtistToken)) +
        geom_col()
      gg <- gg +
        labs(title = paste("Annual Plays by Artist"), caption = HOST_URL)
      gg <- gg + theme_economist()
      #gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
      gg <- gg + scale_x_continuous()
      #gg<-gg+ theme(plot.background = element_rect(fill="black"))
      gg
    },
    bg = "black"
  )

  output$multi_artist_history_plot_2 <- renderPlot(
    {
      multi_artist_history <- reactive_multi_artists()
      gg <- multi_artist_history |>
        ggplot(aes(x = AirDate, y = Spins, fill = ArtistToken)) +
        geom_col()
      gg <- gg +
        labs(title = paste("Annual Plays by Artist"), caption = HOST_URL)
      #gg<- gg+ theme_economist()
      gg <- gg +
        theme_solarized_2(light = FALSE) +
        scale_colour_solarized("red")
      gg <- gg +
        theme(
          plot.background = element_rect(fill = "black"),
          legend.background = element_rect(fill = "black")
        )
      gg <- gg + scale_x_continuous()
      gg <- gg + theme(legend.position = "top")
      gg
    },
    bg = "black"
  )

  output$multi_artist_history_plot_4 <- renderPlot(
    {
      multi_artist_history <- reactive_multi_artists()
      gg <- multi_artist_history |>
        ggplot(aes(x = AirDate, y = Spins, fill = ArtistToken)) +
        geom_col()
      gg <- gg + facet_grid(~ArtistToken)
      gg <- gg +
        labs(title = paste("Annual Plays by Artist"), caption = HOST_URL)
      #gg<- gg+ theme_economist()
      gg <- gg +
        theme_solarized_2(light = FALSE) +
        scale_colour_solarized("red")
      gg <- gg + theme(plot.background = element_rect(fill = "black"))
      gg <- gg + scale_x_continuous() + theme(legend.position = "none")

      gg
    },
    bg = "black"
  )

  # ------------------ SONG TAB -----------------
  # Type-ahead: the selectize `load` callback fetches matches from a
  # per-session data endpoint backed by the DuckDB song_index table.
  song_search_url <- session$registerDataObj(
    "song_search",
    NULL,
    function(data, req) {
      q <- shiny::parseQueryString(req$QUERY_STRING)$query
      res <- song_choices(search_songs(if (is.null(q)) "" else q))
      structure(
        list(
          status = 200L,
          content_type = "application/json",
          content = enc2utf8(jsonlite::toJSON(res)),
          headers = list(`X-Content-Type-Options` = "nosniff")
        ),
        class = "httpResponse"
      )
    }
  )
  # initial choice/selection come from the UI; here we only install the
  # type-ahead loader, which needs this session's endpoint URL. Assign with
  # `$<-` (not c()) so the I() marker survives and Shiny evals the JS.
  song_opts <- song_selectize_options
  song_opts$load <- I(sprintf(
    "function(query, callback) {
       if (query.length < %d) return callback();
       fetch('%s&query=' + encodeURIComponent(query))
         .then(function(r) { return r.json(); })
         .then(callback)
         .catch(function() { callback(); });
     }",
    song_search_min_chars,
    song_search_url
  ))
  updateSelectizeInput(session, "song_selection", options = song_opts)

  # one DuckDB pass per (titles, years); threshold / wake toggles are R-only
  song_plays <- reactive({
    req(input$song_selection)
    withProgress(message = "Processing...", {
      get_song_plays(input$song_selection, input$song_years_range)
    })
  })

  output$song_history_plot <- renderPlot(
    {
      song_history <- quarterly_by_dj(
        song_plays(),
        threshold = as.numeric(input$song_all_other),
        exclude_wake = input$exclude_wake_songs,
        exclude_signature = input$exclude_signature_songs
      )
      gg <- song_history |>
        ggplot(aes(x = AirDate, y = Spins, fill = ShowName)) +
        geom_col(orientation = "x") +
        scale_x_yearqtr(format = "%Y", guide = guide_axis(check.overlap = TRUE))
      gg <- gg +
        labs(
          title = paste(
            "Number of",
            paste(input$song_selection, collapse = ", "),
            "plays every quarter by DJ"
          ),
          x = "",
          caption = HOST_URL
        )
      gg <- gg +
        theme_solarized_2(light = FALSE) +
        scale_colour_solarized("red")
      gg <- gg +
        theme(
          plot.background = element_rect(fill = "black"),
          legend.background = element_rect(fill = "black")
        )
      gg
    },
    bg = "black"
  )
  output$top_artists_for_song <- renderTable({
    song_top_artists(
      song_plays(),
      exclude_wake = input$exclude_wake_songs,
      exclude_signature = input$exclude_signature_songs
    )
  })

  # ------------------- playlists TAB--------------------
  playlist_dj <- reactive({
    djKey[djKey$ShowName == input$show_selection_5, ][1, ]
  })

  # new show -> default to its most recent month (UI already holds Ken's)
  observeEvent(input$show_selection_5, ignoreInit = TRUE, {
    last <- playlist_dj()$LastShow
    updateDateRangeInput(
      session,
      "playlist_date_range",
      start = last - 30,
      end = last
    )
  })
  observeEvent(input$reset_playlist_date_range, {
    dj <- playlist_dj()
    updateDateRangeInput(
      session,
      "playlist_date_range",
      start = dj$FirstShow,
      end = dj$LastShow
    )
  })

  output$dj_playlist_link <- renderUI({
    a(
      "DJ's Archived Shows at WFMU.org",
      href = paste0("https://wfmu.org/playlists/", playlist_dj()$DJ),
      target = "_blank",
      rel = "noopener noreferrer",
      style = "
             border-radius: 25px;
             padding: 5px;
             background-color: teal;
             color: white"
    )
  })

  playlist_data <- reactive({
    rng <- input$playlist_date_range
    req(length(rng) == 2, !anyNA(rng), rng[1] <= rng[2])
    withProgress(message = "Fetching playlist...", {
      get_playlist(playlist_dj()$DJ, rng[1], rng[2])
    })
  })

  output$playlist_table <- DT::renderDataTable({
    df <- playlist_data()
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Title = "No shows in this date range."),
        style = "bootstrap4",
        rownames = FALSE
      ))
    }
    # a character marker (rather than TRUE/FALSE) keeps the column sortable
    # and lets users type the star into the search box
    df$Signature <- ifelse(df$Signature, "\u2605", "")
    datatable(
      df,
      style = "bootstrap4",
      rownames = FALSE,
      options = list(
        pageLength = 25,
        deferRender = TRUE,
        columnDefs = list(list(className = "dt-center", targets = 3))
      )
    ) |>
      formatStyle(
        c("AirDate", "Artist", "Title", "Signature"),
        valueColumns = "Signature",
        color = styleEqual("\u2605", "#f39c12"),
        fontStyle = styleEqual("\u2605", "italic")
      )
  })
}
# LAUNCH APP ===============================================================
shinyApp(ui, server)
