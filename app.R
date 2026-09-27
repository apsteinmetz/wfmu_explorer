# WFMU explorer verion 1.1
# ----------------- LOAD LIBRARIES ----------------------
# options for dev or for deployment
Sys.setenv(DUCKPLYR_FORCE = FALSE)
options(shiny.minified = TRUE)
options(shiny.autoreload = FALSE)
options("dplyr.summarise.inform" = FALSE)
options(duckdb.materialize_message = FALSE)
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
load('data/similarity_histogram_gg.rdata') # precomputed histogram ggplot object "gg_sim"
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

channel_names <- unique(djKey$Channel)

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
    years_range = c(2010, 2023)
  ) {
    # duckplyr needs plain scalars in filter expressions (no `x[1]` indexing)
    yr <- ytd(years_range)
    y1 <- yr[1]
    y2 <- yr[2]
    dj_codes <- station_dj_codes(channel, exclude_wake, exclude_bots)

    # one filtered base relation shared by both aggregates; semi_join because
    # duckplyr won't translate `%in%` against a long vector
    base <- playlists |>
      select(DJ, AirDate, ArtistToken, Title) |>
      filter(AirDate >= y1, AirDate <= y2) |>
      semi_join(tibble(DJ = dj_codes), by = "DJ")

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
  cache = cachem::cache_mem(max_size = 20 * 1024^2, max_age = 24 * 3600)
)

# prime the cache with the Station tab's default view so the first visitor
# doesn't wait for it
station_defaults <- list(
  channel = "ALL",
  exclude_wake = FALSE,
  exclude_bots = TRUE,
  years_range = c(max_year - 3, max_year)
)
invisible(do.call(get_station_stats, station_defaults))

# ----------------- DJ TAB QUERIES ----------------------
# DJ Profile: one filtered base relation feeds both aggregates
get_dj_stats <- memoise(
  function(dj = "TW", years_range = c(2017, 2019)) {
    yr <- ytd(years_range)
    y1 <- yr[1]
    y2 <- yr[2]
    base <- playlists |>
      select(DJ, AirDate, ArtistToken, Title) |>
      filter(DJ == dj, AirDate >= y1, AirDate <= y2)
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
  cache = cachem::cache_mem(max_size = 30 * 1024^2, max_age = 24 * 3600)
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
  cache = cachem::cache_mem(max_size = 10 * 1024^2, max_age = 24 * 3600)
)

get_sim_index <- memoise(
  function(dj1 = "TW", dj2 = "CF") {
    djSimilarity |>
      filter(DJ1 == dj1, DJ2 == dj2) |>
      pull(Similarity)
  },
  cache = cachem::cache_mem(max_size = 5 * 1024^2, max_age = 24 * 3600)
)

# Compare Two DJs: a single DuckDB pass over both DJs' plays, then derive the
# artists-in-common and songs-in-common tables in R. `agg` is transient
# (tens of thousands of rows); only the two 10-row results are cached.
compare_djs <- memoise(
  function(dj1 = "TW", dj2 = "CF") {
    agg <- playlists |>
      filter(DJ %in% c(dj1, dj2)) |>
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
  cache = cachem::cache_mem(max_size = 10 * 1024^2, max_age = 24 * 3600)
)

# Lightweight version of the precomputed similarity histogram: keep the
# binned bars, drop the ~230k raw pair similarities so each render is cheap.
gg_sim_light <- local({
  bars <- ggplot2::layer_data(gg_sim, 1)
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
    labs(x = gg_sim$labels$x, y = gg_sim$labels$y, title = gg_sim$labels$title) +
    # base_size 14 = default 12 + 2 pt for all text elements
    theme_solarized_2(light = FALSE, base_size = 14) +
    theme(plot.background = element_rect(fill = "black"))
})

# ----------------- ARTIST TAB QUERIES ----------------------
# One DuckDB pass per (tokens, years), shared by Single and Multi Artist and
# collapsed to (AirDate, DJ, ArtistToken, Title) counts. Threshold, Wake 'n'
# Bake exclusion and the quarterly/yearly rollups are then cheap R steps on
# the cached result, so toggling those controls never touches DuckDB.
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
      summarise(.by = c(AirDate, DJ, ArtistToken, Title), n = n()) |>
      collect()
  },
  cache = cachem::cache_mem(max_size = 50 * 1024^2, max_age = 24 * 3600)
)

get_artist_variants <- memoise(
  function(tokens) {
    playlists |>
      filter(ArtistToken %in% tokens) |>
      distinct(Artist) |>
      arrange(Artist) |>
      collect()
  },
  cache = cachem::cache_mem(max_size = 10 * 1024^2, max_age = 24 * 3600)
)

# Pure-R rollups of get_artist_plays() output (plain tibbles, no DuckDB).
# Date conversions are done with base R assignment so duckplyr never sees
# as.yearqtr()/year() and no methods_restore() toggling is needed.
drop_wake <- function(plays, exclude_wake) {
  if (exclude_wake) plays[plays$DJ != "WA", ] else plays
}

artist_quarterly_by_dj <- function(plays, threshold = 3, exclude_wake = FALSE) {
  plays <- drop_wake(plays, exclude_wake)
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

artist_top_songs <- function(plays, exclude_wake = FALSE) {
  drop_wake(plays, exclude_wake) |>
    summarise(.by = Title, count = as.integer(sum(n))) |>
    arrange(desc(count))
}

artist_yearly <- function(plays, exclude_wake = FALSE) {
  plays <- drop_wake(plays, exclude_wake)
  plays$AirDate <- year(plays$AirDate)
  plays |>
    summarise(.by = c(AirDate, ArtistToken), Spins = as.integer(sum(n))) |>
    arrange(AirDate)
}

#  DEFINE USER INTERFACE ===============================================================
ui <- {
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
              "NOTE: The Channel Selector filters for DJs currently on that channel. ",
              "This will include all the DJ's shows in the selected date range, even if ",
              "their show used to be on a different channel or if their current channel ",
              "did not exist during the selected date range."
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
            hr(),
            # diplay a clickable url
            uiOutput("dj_profile_link"),
            hr(),
            htmlOutput("other_show_names"),
            hr(),
            uiOutput("DJ_date_slider"),
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
          h4('1) Start by narrowing down the list of songs.'),
          h4('Type all or part of the song name then click "Find Songs."'),
          textInput(
            "song_letters",
            label = h4("Give me a clue!"),
            value = default_song
          ),
          actionButton("song_update_1", "Find Songs"),
          h4('2) Click below to choose the specific song(s).'),
          h5('You can select more than one'),
          uiOutput('SelectSong'),
          h4('3) Change the date range?'),
          sliderInput(
            "song_years_range",
            "Year Range:",
            min = min_year,
            max = max_year,
            sep = "",
            value = c(2002, max_year)
          ),
          fluidRow(
            h4('4) Change threshold to show DJ?'),
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
              start = as.Date("2024-01-02"),
              end = as.Date("2024-02-01"),
              min = min_date,
              max = max_date
            ),
            actionButton(
              "reset_playlist_date_range",
              "Reset Dates to Full History",
              color = "blue"
            ),
            h4("Important Note:"),
            h5("I have stripped out signature songs"),
            h5("that a DJ might play every show"),
            h5("as it distorts the overall popularity"),
            h5("measures in the data set.")
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
          input$years_range_1
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
  # (artist_quarterly_by_dj, artist_top_songs, artist_yearly) are global.

  # ---------------FUNCTIONS FOR SONG TAB -----------------------------
  song_play_count_by_DJ <- memoise(function(
    songs = "Changes",
    years_range = c(2010, 2019),
    threshold = 3,
    exclude_wake = FALSE
  ) {
    years_range <- ytd(years_range)
    y1 <- years_range[1]
    y2 <- years_range[2]
    pc <- playlists |>
      filter(AirDate >= y1) |>
      filter(AirDate <= y2) |>
      filter(Title %in% songs) |>
      as_tibble()

    if (exclude_wake) {
      pc <- pc |>
        filter(DJ != "WA")
    }
    methods_restore()
    pc <- pc |> mutate(AirDate = as.yearqtr(AirDate))

    pc <- pc |>
      summarise(.by = c(AirDate, DJ), Spins = n()) |>
      mutate(DJ = if_else(Spins < threshold, "AllOther", DJ)) |>
      summarise(.by = c(AirDate, DJ), Spins = sum(Spins))

    pc <- pc |>
      left_join(djKey, by = 'DJ') |>
      select(AirDate, Spins, ShowName) |>
      mutate(ShowName = if_else(is.na(ShowName), "AllOther", ShowName)) |>
      arrange(AirDate)
    methods_overwrite()

    return(pc)
  })

  top_artists_for_song <- memoise(function(
    song = "Help",
    years_range = c(2010, 2019)
  ) {
    years_range <- ytd(years_range)
    y1 <- years_range[1]
    y2 <- years_range[2]
    ts <- playlists |>
      filter(AirDate >= y1) |>
      filter(AirDate <= y2) |>
      filter(Title %in% song) |>
      summarise(.by = c(ArtistToken), count = n()) |>
      arrange(desc(count))
    return(ts)
  })

  # ------------------ stuff for playlists tab --------------
  get_playlists <- memoise(function(
    show = "Ken",
    date_range = c(as.Date("2024-01-02"), as.Date("2024-02-01"))
  ) {
    d1 = date_range[1]
    d2 = date_range[2]
    subset_playlists <- djKey |>
      filter(ShowName %in% show) |>
      select(DJ) |>
      left_join(playlists, by = "DJ") |>
      select(-ArtistToken) |>
      filter(AirDate >= d1) |>
      filter(AirDate <= d2) |>
      select(-DJ)
    # print(date_range) # DEBUG
    if (nrow(subset_playlists) == 0) {
      subset_playlists <- data.frame(Title = "No shows in this date range.")
    }
    return(subset_playlists)
  })

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
      get_dj_stats(dj_profile()$DJ, dj_years())
    })
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
      compare_djs(p[1], p[2])
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
      artist_history <- artist_quarterly_by_dj(
        artist_plays_1DJ(),
        threshold = as.numeric(input$artist_all_other_1DJ),
        exclude_wake = input$exclude_wake_artists
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
    artist_top_songs(artist_plays_1DJ(), input$exclude_wake_artists)
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
    artist_yearly(artist_plays_multi(), input$exclude_wake_artists_multi)
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
  reactive_songs_letters <- reactive({
    input$song_update_1
    isolate({
      song_letters <- str_to_title(input$song_letters)
      withProgress({
        setProgress(message = "Processing...")
        ret_val <- playlists |>
          filter(grepl(song_letters, Title)) |>
          select(Title) |>
          distinct() |>
          arrange(Title)
      })
    })
    return(ret_val)
  })

  process_songs <- function() {
    withProgress({
      setProgress(message = "Processing...")
      ret_val <- song_play_count_by_DJ(
        input$song_selection,
        input$song_years_range,
        as.numeric(input$song_all_other),
        input$exclude_wake_songs
      )
    })
    return(ret_val)
  }

  output$SelectSong <- renderUI({
    song_choices <- reactive_songs_letters()
    selectizeInput(
      "song_selection",
      h5("Select song"),
      selected = "Help",
      choices = song_choices,
      multiple = TRUE
    )
  })
  output$song_history_plot <- renderPlot(
    {
      song_history <- process_songs()
      gg <- song_history |>
        ggplot(aes(x = AirDate, y = Spins, fill = ShowName)) +
        geom_col(orientation = "x")
      scale_x_yearqtr(format = "%Y", guide = guide_axis(check.overlap = TRUE))
      gg <- gg +
        labs(
          title = paste(
            "Number of",
            input$song_selection,
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
    top_artists_for_song(input$song_selection, input$song_years_range)
  })

  # ------------------- playlists TAB--------------------
  observeEvent(input$reset_playlist_date_range, {
    ss5 <- input$show_selection_5
    ss5_key <- filter(djKey, ShowName == ss5)
    updateDateRangeInput(
      session = session,
      inputId = "playlist_date_range",
      start = pull(ss5_key, FirstShow),
      end = pull(ss5_key, LastShow),
      # min = ss5 |> pull(FirstShow),
      # max = ss5 |> pull(LastShow)
    )
  })
  output$dj_playlist_link <- renderUI({
    ss5 <- input$show_selection_5
    DJ <- filter(djKey, ShowName == ss5) |>
      pull(DJ)
    playlist_URL <- paste0("https://wfmu.org/playlists/", DJ)
    url <- a(
      "DJ's Archived Shows at WFMU.org",
      href = playlist_URL,
      target = "_blank",
      rel = "noopener noreferrer",
      style = "
             border-radius: 25px;
             padding: 5px;
             background-color: teal;
             color: white"
    )
    tagList(url)
    tagList(url)
  })

  # output$playlist_table<-DT::renderDataTable({
  #   datatable(get_playlists(input$show_selection_5,input$playlist_date_range),
  #             style = "bootstrap",
  #             options = list(initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "$(this.api().table().body()).css({'background-color': '#000', 'color': '#44d'});",
  #               "}")
  #             )
  #   )
  # })
  output$playlist_table <- DT::renderDataTable({
    datatable(
      get_playlists(input$show_selection_5, input$playlist_date_range),
      style = "bootstrap4"
    )
  })
  # WHY WONT GT WORK
  #   output$playlist_table<-gt::render_gt({
  #     get_playlists(input$show_selection_5,input$playlist_date_range) |> gt() |>
  # #     get_playlists() |> gt() |>
  #       tab_header(title = "Playlist") |>
  #       opt_stylize(style=2,color = "green")
  #    })
  # output$playlist_table<-renderDataTable({
  #   get_playlists(input$show_selection_5,input$playlist_date_range)
  # })
}
# LAUNCH APP ===============================================================
shinyApp(ui, server)
