# TEST BSLIB
# WFMU explorer verion 0.9
# ----------------- LOAD LIBRARIES ----------------------
library(dplyr)
library(tidyr)
library(jsonlite)
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
library(duckdb)
library(duckplyr)
library(gt)
library(DT)
library(bslib)

options("dplyr.summarise.inform"= FALSE)
options(duckdb.materialize_message = FALSE)
methods_overwrite()

load('data/djdtm.rdata') # document term object for similarity
playlists <- duckplyr_df_from_file('data/playlists.parquet',"read_parquet")
djKey <- duckplyr_df_from_file('data/djKey.parquet',"read_parquet") |> 
  # preserve only unique DJs
  distinct(DJ, .keep_all = TRUE)|> 
  arrange(ShowName)
djSimilarity <- duckplyr_df_from_file('data/dj_similarity_tidy.parquet',"read_parquet")
djDistinctive <- duckplyr_df_from_file('data/distinctive_artists.parquet',"read_parquet")

source("wordcloud2a.R")

# ----------------- DO SETUP ----------------------
HOST_URL<- "wfmu.artsteinmetz.com"
#playlists <- playlists %>% mutate_if(is.character,str_squish)
default_song<-"Help"
default_artist<-'Abba'
default_artist_multi<-c('Abba','Beatles')

max_date <- max(playlists$AirDate)
min_date <- min(playlists$AirDate)
max_year<-max(year(max_date))
min_year<-min(year(min_date))

# convert years range to date range
ytd <- function(years_range) {
  years_range <- c(as.Date(paste0(round(years_range[1]), "-1-1")),
                   as.Date(paste0(round(years_range[2]), "-12-31")))
  return(years_range)
}
#limit DJ list to DJs that are present in playlist file

djKey <- select(playlists,DJ) %>% distinct() %>% left_join(djKey)

all_artisttokens <- distinct(select(playlists,ArtistToken)) %>% pull()

# SIDEBARS ---------------------------------------------------------------------
sidebar_station = sidebar(
  selectInput(
    "selection",
    "Are the DJs On Current Schedule?:",
    choices = c('ALL', 'YES', 'NO'),
    selectize = TRUE,
    selected = "ALL"
  ),
  checkboxInput("exclude_wake", "Exclude Wake 'n' Bake?", value = FALSE),
  helpText('Be aware a wide date range could take many seconds to process.'),
  sliderInput(
    "years_range_1",
    "Year Range:",
    min = min_year,
    max = max_year,
    sep = "",
    step = 1,
    round = TRUE,
    value = c(max_year - 3, max_year)
  ) ,
  textOutput("play_count"),
  h2(),
  actionButton("update", "Update View")
)

sidebar_dj1 = sidebar(
  selectInput("show_selection", "Show Name:",
              choices = sort(djKey$ShowName),
              selected = "Diane's Kamikaze Fun Machine"),
  hr(),
  # diplay a clickable url
  uiOutput("dj_profile_link"),
  hr(),
  uiOutput("DJ_date_slider"),
  hr(),
  h4('Distinctive Artists'),
  h4('Artists that are played often by this DJ but seldom by others'),
  tableOutput("DJ_table_distinct_artists"),
  hr(),
  h5("If you're curious, this is a term frequency - inverse document frequency (TF-IDF) analysis.")
)

sidebar_dj2 = sidebar(
  selectInput("show_selection_2", "Show Name:",
              choices = sort(djKey$ShowName),
              selected = "Diane's Kamikaze Fun Machine")
)

sidebar_artist1 = sidebar(sliderInput(
  "bins",
  "Number of bins:",
  min = 1,
  max = 50,
  value = 30
))

sidebar_artist2 = sidebar(sliderInput(
  "bins",
  "Number of bins:",
  min = 1,
  max = 50,
  value = 30
))

sidebar_songs = sidebar(sliderInput(
  "bins",
  "Number of bins:",
  min = 1,
  max = 50,
  value = 30
))

# CARDS ------------------------------------------------------------------------
card_station = card(
    h3(HTML(paste0(
      '<span style="white-space: nowrap">',
      "Top Artists and Songs Played on ",
      a("WFMU", href = "https://wfmu.org"),
      "</span>"
    ))),
    layout_sidebar(
      sidebar = sidebar_station,
      card(
        card_header("Top Artists"),
        card_body(
          page_navbar(
            nav_panel(title = "WordCloud",
                      "artist wordcloud"),
            nav_panel(title = "Table",
                      "artistTable")
          )),
        card(
          card_header("Top Songs"),
          "songTable"
        )
      )
    )
)
card_dj1 = card(
  h3("DJ Profile"),
  layout_sidebar(
    sidebar = sidebar_dj1,
    card(
      card_header("Top Artists"),
      card_body(page_navbar(
        nav_panel(title = "WordCloud",
              "artist wordcloud"),
        nav_panel(title = "Table",
              "artistTable")
  )),
    card(
      card_header("Top Songs"),
      tableOutput("distTable"))
)))

card_dj2 = card(
  h3("Find Similar DJs"),
  layout_sidebar(
    sidebar = sidebar_dj2,
    card(
      card_header("DJ Neigborhood"),
      withSpinner(plotOutput("DJ_chord"))
      ),
    card(
        card_header('Most Similar Shows Based on Common Artists'),
        tableOutput("DJ_table_similar")
    ))
  )

card_dj3 = card(
  h3("Compare Two DJs"),
#   column(4,
#          selectInput("show_selection_1DJ", "Show Name:",
#                      choices = sort(djKey$ShowName),
#                      selected = "Diane's Kamikaze Fun Machine")
#   ),
#   column(4,
#          selectInput("show_selection_4", "Show Name:",
#                      choices = sort(djKey$ShowName),
#                      selected = 'Bob Brainen')
#   )
# ),
# fluidRow(
#   column(6,
#          h4('Similarity Index'),
#          h5('Bars are the  frequency of all DJ pair similarities. Vertical line is similarity of this pair.'),
#          h5(' The bulge at the low end shows WFMU DJs are not very similar to each other, in general.')
#   ),
#   column(6,
#          withSpinner(plotOutput("DJ_plot_sim_index",height="100px"))
#   )
# ),
# fluidRow(column(11,offset=1,h4("Play Counts of Common Artists and Songs"))),
# fluidRow(
#   column(5,
#          h4('Artists in Common'),
#          tableOutput("DJ_table_common_artists")
#   ),
#   column(7,
#          h4('Songs in Common'),
#          tableOutput("DJ_table_common_songs")
#   )
# )
# )
)

# UI ---------------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(bootswatch = "shiny"),
  page_navbar(
    title = "WFMU Playlist Explorer",
    nav_panel(title = "One",
              card_station),
    nav_menu(title = "DJs", 
             nav_panel(title = "DJ Profile", card_dj1), 
             nav_panel(title = "Find Similar DJs",card_dj2),
             nav_panel(title = "Compare Two DJs",card_dj3)
    ),
    nav_panel(title = "Artists", 
              "stuff"),
    nav_panel(title = "Three", 
              p("Third tab content")),
    nav_spacer(),
    nav_item(input_dark_mode(mode = "dark")),
  )
)

# server -----------------------------------------------------------------------
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(round(runif(100,max = 100)))
  })
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)

# bs_theme_preview()
