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

options("dplyr.summarise.inform"=FALSE)
options(duckdb.materialize_message = FALSE)
methods_overwrite()

load('data/djdtm.rdata') # document term object for similarity
playlists <- duckplyr_df_from_file('data/playlists.parquet',"read_parquet")
DJKey <- duckplyr_df_from_file('data/DJKey.parquet',"read_parquet")
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

DJKey <- select(playlists,DJ) |> distinct() |> left_join(DJKey)

all_artisttokens <- distinct(select(playlists,ArtistToken)) |> pull()

#  DEFINE USER INTERFACE ===============================================================
ui <- {
  navbarPage("WFMU Playlist Explorer BETA VERSION 0.9",theme = shinytheme("darkly"),
                 # -- Add Tracking JS File 
                 #rest of UI doesn't initiate unless tab is clicked on if the code below runs
                 #tags$head(includeScript("google-analytics.js"))
             
                # *-------- Station TAB ----------------------------------
                 tags$head(includeHTML(("google-analytics.html"))),
                 # --------- Station TAB ----------------------------------
                 tabPanel("Station",
                          titlePanel("Top Artists and Songs Played on WFMU"),
                          fluidPage(
                            # ---- Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("selection", "Are the DJs On Current Schedule?:",
                                            choices = c('ALL','YES','NO'),
                                            selectize = TRUE,
                                            selected = "ALL"),
                                helpText('Be aware a wide date range could take many seconds to process.'),
                                sliderInput("years_range_1",
                                            "Year Range:",
                                            min = min_year,
                                            max = max_year,
                                            sep = "",
                                            step=1,
                                            round= TRUE,
                                            value = c(max_year-3,max_year)) ,
                                textOutput("play_count"),
                                h2(),
                                actionButton("update", "Update View")
                              ),
                              # ---------- Main panel for displaying outputs ----
                              mainPanel(
                                h4("Top Artists"),
                                tabsetPanel(type = "tabs",
                                            tabPanel("Word Cloud",
                                                     withSpinner(wordcloud2Output("cloud"))) ,
                                            tabPanel("Table", tableOutput("table_artists"))
                                ),
                                h4("Songs"),
                                tableOutput("table_songs")
                              )
                            )
                          )
                 ),
                # *-------- DJ TAB ----------------------------------
                 navbarMenu("DJs",
                            # --------- DJs/DJ Profile -----------------------------
                            tabPanel("DJ Profile",
                                     titlePanel("DJ Profile"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("show_selection", "Show Name:",
                                                     choices = DJKey$ShowName,
                                                     selected = "Diane's Kamikaze Fun Machine"),
                                         hr(),
                                         uiOutput("DJ_date_slider"),
                                         #, actionButton("DJ_update","Update")
                                         hr(),
                                         h4('Distinctive Artists'),
                                         h4('Artists that are played often by this DJ but seldom by others'),
                                         tableOutput("DJ_table_distinct_artists"),
                                         hr(),
                                         h5("If you're curious, this is a term frequency - inverse document frequency (TF-IDF) analysis.")
                                       ),

                                       # Show Word Cloud
                                       mainPanel(
                                         fluidRow(
                                           h4('Top Artists'),
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Word Cloud",
                                                                withSpinner(wordcloud2Output("DJ_cloud"))),
                                                       tabPanel("Table", tableOutput("DJ_table_artists"))
                                           )),
                                         fluidRow(
                                           h4('Top Songs'),
                                           tableOutput("DJ_table_songs")
                                         )
                                       )
                                     )
                            ),
                            # --------- DJs/Find Simlar DJs -------------------
                            tabPanel("Find Similar DJs",
                                     titlePanel("Find Similar DJs"),
                                     sidebarLayout(
                                       # Sidebar with a slider and selection inputs
                                       sidebarPanel(
                                         selectInput("show_selection_2", "Show Name:",
                                                     choices = DJKey$ShowName,
                                                     selected = "Diane's Kamikaze Fun Machine")
                                       ),

                                       # Show Word Cloud
                                       mainPanel(
                                         fluidRow(
                                           h4('DJ Neighborhood')
                                           , withSpinner(plotOutput("DJ_chord"))
                                         ),
                                         fluidRow(
                                           h4('Most Similar Shows Based on Common Artists'),
                                           tableOutput("DJ_table_similar")
                                         )
                                       )
                                     )
                            ),
                            # --------- DJs/Compare Two DJs -----------------------
                            tabPanel("Compare Two DJs",
                                     titlePanel("Compare Two DJs"),
                                     fluidRow(
                                       column(4,
                                              selectInput("show_selection_1DJ", "Show Name:",
                                                          choices = DJKey$ShowName,
                                                          selected = "Diane's Kamikaze Fun Machine")
                                       ),
                                       column(4,
                                              selectInput("show_selection_4", "Show Name:",
                                                          choices = DJKey$ShowName,
                                                          selected = 'Bob Brainen')
                                       )
                                     ),
                                     fluidRow(
                                       column(6,
                                              h4('Similarity Index'),
                                              h5('Bars are the  frequency of all DJ pair similarities. Vertical line is similarity of this pair.'),
                                              h5(' The bulge at the low end shows WFMU DJs are not very similar to each other, in general.')
                                       ),
                                       column(6,
                                              withSpinner(plotOutput("DJ_plot_sim_index",height="100px"))
                                       )
                                     ),
                                     fluidRow(column(11,offset=1,h4("Play Counts of Common Artists and Songs"))),
                                     fluidRow(
                                       column(5,
                                              h4('Artists in Common'),
                                              tableOutput("DJ_table_common_artists")
                                       ),
                                       column(7,
                                              h4('Songs in Common'),
                                              tableOutput("DJ_table_common_songs")
                                       )
                                     )
                            )
                 ), # end DJ tab
                # *-------- ARTISTS TAB ----------------------------------
                 navbarMenu("Artists",
                            #----------- Single Artist -----------------------
                            tabPanel("Single Artist",
                                     titlePanel("Artists Plays by DJ Over Time"),
                                     sidebarLayout(
                                       # Sidebar with a slider and selection inputs
                                       sidebarPanel(
                                         fluidRow(
                                           h4('Artist names reduced to token of first two words.'),
                                           h4("Select one or more artists"),
                                           selectizeInput("artist_selection_1DJ",
                                                          label = NULL,
                                                          choices = NULL,
                                                          multiple = TRUE
                                           ),
                                           h4('Change the date range to include?'),
                                           sliderInput("artist_years_range_1DJ",
                                                       "Year Range:",
                                                       min = min_year,
                                                       max = max_year,
                                                       sep = "",
                                                       step=1,
                                                       round=TRUE,
                                                       value = c(2002,max_year)
                                                       ),
                                           h4('Change threshold to show DJ name?'),
                                           selectInput("artist_all_other_1DJ",
                                                       "Threshold of Minimum Plays to show DJ",
                                                       selected = 3,
                                                       choices=1:9),
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
                            tabPanel("Multi Artist",
                                     titlePanel("Multi-Artist Plays Over Time"),
                                     sidebarLayout(
                                       # Sidebar with a slider and selection inputs
                                       sidebarPanel(
                                         fluidRow(
                                           h4('Artist names reduced to token of first two words.'),
                                           selectizeInput("artist_selection_multi", h4("Select two or more artists"),
                                                          choices = NULL,
                                                          multiple = TRUE,
                                                          #selected=default_artist_multi,
                                                          options = list(closeAfterSelect=TRUE)
                                                          #options = list(placeholder = 'select artist(s)')
                                           ),
                                           h4('Change the date range to include?'),
                                           sliderInput("artist_years_range_multi",
                                                       "Year Range:",
                                                       min = min_year,
                                                       max = max_year,
                                                       sep = "",
                                                       step=1,
                                                       round=TRUE,
                                                       value = c(2002,max_year)),
                                           h4('Full artist names included in these tokens:'),
                                           tableOutput("artist_variants_multi")
                                         )
                                       ),

                                       mainPanel(
                                         fluidRow(
                                           h4("Artist Plays Per Year."),
                                           withSpinner(plotOutput("multi_artist_history_plot_4",width = "710px",height="355px")),
                                           h4('Artist Plays per Year (another way)'),
                                           plotOutput("multi_artist_history_plot_2",width = "710px",height="355px"),
                                           h4('Artist Plays per Year (light version)'),
                                           h4('(The way Ken Likes to see it for WFMU site).'),
                                           plotOutput("multi_artist_history_plot",width = "710px",height="355px"),
                                           h4()
                                         )
                                       )

                                     )

                            )
                 ),
                # *-------- SONGS TAB ----------------------------------
                 tabPanel("Songs",
                          titlePanel("Find Songs"),
                          sidebarLayout(
                            # Sidebar with a slider and selection inputs
                            sidebarPanel(
                              h4('1) Start by narrowing down the list of songs.'),
                              h4('Type all or part of the song name then click "Find Songs."'),
                              textInput("song_letters", label = h4("Give me a clue!"), value = default_song),
                              actionButton("song_update_1","Find Songs"),
                              h4('2) Click below to choose the specific song(s).'),
                              h5('You can select more than one'),
                              uiOutput('SelectSong'),
                              h4('3) Change the date range?'),
                              sliderInput("song_years_range",
                                          "Year Range:",
                                          min = min_year,
                                          max = max_year,
                                          sep = "",
                                          value = c(2002,max_year)),
                              fluidRow(
                                h4('4) Change threshold to show DJ?'),
                                selectInput("song_all_other",
                                            "Threshold of Minimum Plays to show DJ",
                                            selected = 3,
                                            choices=1:9)
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
                 tabPanel("Playlists",
                          titlePanel("Get a Playlist"),
                          sidebarLayout(
                            sidebarPanel(
                              fluidRow(
                                h4("Choose a Show:"),
                                selectInput("show_selection_5", "Show Name:",
                                            choices = DJKey$ShowName,
                                            selected = "Diane's Kamikaze Fun Machine"),
                                h4('Choose Date Range:'), #just to make some space for calendar
                                dateRangeInput("playlist_date_range",
                                               "Date Range:",
                                               start = as.Date("2017-01-02"),
                                               end = as.Date("2017-02-01"),
                                               min = min_date,
                                               max = max_date
                                ),
                                actionButton("reset_playlist_date_range",
                                             "Reset Dates to Full History",
                                             color="blue"),
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
                 tabPanel("About",
                          mainPanel(
                            includeMarkdown("about.md")
                          )
                 )

 ) # end UI
 
}
# DEFINE SERVER ===============================================================
server <- function(input, output, session) {
  # -------------- FUNCTIONS FOR STATION TAB -----------------------------
  get_top_artists<-function(onAir="ALL",years_range = c(2010,2023)) {
    years_range <- ytd(years_range)
    if (onAir=='ALL') {
      DJ_set <- DJKey %>% 
        select(DJ)   } else {
          DJ_set <-DJKey %>% 
            filter(onSched==onAir) %>% #on Sched or off?
            select(DJ) 
        }
    
    top_artists<-DJ_set %>% 
      left_join(playlists,by='DJ') %>%
      filter(ArtistToken != "Unknown") %>% 
      filter(ArtistToken != "") %>% 
      filter(AirDate>=years_range[1]) %>%  
      filter(AirDate<=years_range[2]) %>%  
      summarize(.by=ArtistToken,play_count=n())%>%
      arrange(desc(play_count)) %>% 
      head(100)
    return(top_artists)
  }
  
  
  get_top_songs<-(function(onAir='ALL',years_range = c(2010,2023)) {
  years_range <- ytd(years_range)
  if (onAir=='ALL') {
    DJ_set <-DJKey %>% 
      select(DJ)
  } else {
    DJ_set <-DJKey %>% 
      filter(onSched==onAir) %>% #on Sched or off?
      select(DJ)
  }  
  songs<-DJ_set %>%  
    left_join(playlists,by='DJ') %>%
    filter(AirDate>=years_range[1]) %>%  
    filter(AirDate<=years_range[2]) %>%  
    filter(Title != "") %>% 
    summarize(.by=c(ArtistToken,Title),play_count=n())%>%
    arrange(desc(play_count))
  
  top_songs <- list(count = nrow(songs),
                    songs = head(songs,25)) 
  return(top_songs)
})

  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  top_artists_reactive<-eventReactive(
    input$update,
    {
      withProgress({
        setProgress(message = "Processing Artists...")
        ret_val <- get_top_artists(input$selection,input$years_range_1)
      })
      return(ret_val)
    },ignoreNULL = FALSE)
  
  top_songs_reactive<-eventReactive(
    input$update,
    {
      withProgress({
        setProgress(message = "Processing Songs...")
        get_top_songs(input$selection,input$years_range_1)
      })
    },
    ignoreNULL = FALSE)
  
  output$test <- renderText({
    paste("test")
  })
  
  # -------------- FUNCTIONS FOR DJS TAB -----------------------------
  get_top_artists_DJ<-memoise(function(dj="TW",years_range = c(2017,2019)) {
    years_range <- ytd(years_range)
    top_artists<-playlists %>%
      filter(DJ==dj) %>% 
      filter(AirDate>=years_range[1]) %>%  
      filter(AirDate<=years_range[2]) %>%  
      summarize(.by = ArtistToken,play_count=n())%>%
      arrange(desc(play_count)) %>% 
      head(100)
    return(as_tibble(top_artists))
  })
  
  get_top_songs_DJ<-memoise(function(dj="TW",years_range = c(2017,2019)) {
    years_range <- ytd(years_range)
    top_songs<-playlists %>% 
      filter(DJ==dj) %>% 
      filter(AirDate>=years_range[1]) %>%
      filter(AirDate<=years_range[2]) %>%
      summarize(.by=c(ArtistToken,Title),play_count=n())%>%
      arrange(desc(play_count)) %>% 
      head(25)
    return(as_tibble(top_songs))
  })
  
  get_similar_DJs<-memoise(function(dj = "TW") {
    similar_DJs<-djSimilarity %>% 
      filter(DJ1==dj) %>% 
      arrange(desc(Similarity)) %>% 
      head(10) %>% 
      rename(DJ=DJ2) %>% 
      select(DJ,Similarity) %>% 
      left_join(DJKey,by='DJ') %>%
      #add target dj to top of table so we see the 2-letter code for the chord chart
      full_join(filter(DJKey,DJ==dj)) %>% 
      mutate(Similarity2 = Similarity) |> 
      mutate(Similarity=paste0(trunc(Similarity*100),"%")) %>% 
      arrange(desc(Similarity2)) |> 
      select(ShowName,DJ,onSched,showCount,Similarity) |> 
      as_tibble()
    return(similar_DJs)
  })
  
  get_sim_index<-memoise(function(dj1 = "TW",dj2 = "CF") {
    DJ_sim<-djSimilarity %>% 
      filter(DJ1==dj1,DJ2==dj2) %>%
      pull(Similarity)
    return(DJ_sim)
  })
  
  artists_in_common<-memoise(function(dj1="TW",dj2="CF"){
    artists<-playlists %>% 
      filter(DJ %in% c(dj1,dj2)) %>% 
      summarise(.by = c(DJ,ArtistToken), n=n()) %>%
      spread(DJ,n) %>% 
      mutate(sum_x=rowSums(.[2:ncol(.)],na.rm=TRUE)) %>% 
      mutate(sd_x=.[2:ncol(.)] %>% replace_na(list(sd_x = 0)) %>% apply(1,sd)) %>% 
      mutate(FaveIndex=trunc(sum_x-1.8*sd_x)) %>% 
      #select(ArtistToken,sum,FaveIndex) %>% 
      arrange(desc(FaveIndex)) %>%
      head(10) %>% 
      select(-sum_x,-sd_x) %>% 
      select(-FaveIndex)
    return(artists)
  })
  
  songs_in_common<-memoise(function(dj1="TW",dj2="CF"){
    songs<-playlists %>% 
      filter(DJ %in% c(dj1,dj2)) %>%
      summarise(.by=c(DJ,ArtistToken,Title),n=n()) %>%
      spread(DJ,n) %>% 
      mutate(sum_x=rowSums(.[3:ncol(.)],na.rm=TRUE)) %>% 
      mutate(sd_x=.[3:ncol(.)] %>% replace_na(list(sd_x = 0)) %>% apply(1,sd)) %>% 
      mutate(FaveIndex=trunc((sum_x-1.8*sd_x)*10)) %>% #apply some arbitrary scaling
      select(-sum_x,-sd_x) %>% 
      arrange(desc(FaveIndex)) %>% 
      select(-FaveIndex) %>% 
      head(10)
    return(songs)
  })
  # ---------------FUNCTIONS FOR ARTIST TAB -----------------------------
  
  play_count_by_DJ<-memoise(function(artist_token = "Abba",years_range = c(2016,2019),threshold=3){
    years_range <- ytd(years_range)
    pc<- playlists %>% 
      filter(AirDate>=years_range[1]) %>%  
      filter(AirDate<=years_range[2]) %>%  
      mutate(DJ=as.character(DJ)) %>% 
      filter(ArtistToken %in% artist_token) %>% 
      mutate(AirDate=as.yearqtr(AirDate))  %>% 
      summarise(.by = c(AirDate,DJ),Spins=n()) |> 
      mutate(DJ = if_else(Spins < threshold, "AllOther", DJ)) |> 
      summarise(.by = c(AirDate,DJ),Spins=sum(Spins))
    
    pc1<-pc %>% 
        left_join(DJKey,by='DJ') %>% 
        select(AirDate,Spins,ShowName) %>%
        mutate(ShowName=if_else(is.na(ShowName),"AllOther",ShowName)) |> 
        arrange(AirDate)

    return(pc1)
  })
  
  play_count_by_artist<-memoise(function(artist_tokens= c("Abba","Beatles"),years_range=c(2012,2015)){
    years_range <- ytd(years_range)
    pc<- playlists %>% 
      ungroup() %>% 
      filter(AirDate>=years_range[1]) %>%  
      filter(AirDate<=years_range[2]) %>%  
      filter(ArtistToken %in% artist_tokens) %>% 
      mutate(AirDate=year(AirDate))  %>% 
      summarise(.by =c(AirDate,ArtistToken),Spins=n()) %>% 
      arrange(AirDate)
    return(pc)
  })
  
  top_songs_for_artist<-memoise(function(artist_token = "Abba",years_range=c(2012,2015)){
    years_range <- ytd(years_range)
    ts<-playlists %>% 
      filter(ArtistToken %in% artist_token) %>% 
      filter(AirDate>=years_range[1]) %>%  
      filter(AirDate<=years_range[2]) %>%  
      summarise(.by =Title,count=n()) %>% 
      arrange(desc(count))
    return(ts)
  })
  
  # ---------------FUNCTIONS FOR SONG TAB -----------------------------
  song_play_count_by_DJ<-memoise(function(songs = "Changes",years_range = c(2010,2019),threshold=3){
    years_range <- ytd(years_range)
    pc<- playlists %>% 
      filter(AirDate>=years_range[1]) %>%  
      filter(AirDate<=years_range[2]) %>%  
      filter(Title %in% songs) %>% 
      mutate(AirDate=as.yearqtr(AirDate))  %>% 
      summarise(.by = c(AirDate,DJ),Spins=n()) %>% 
      mutate(DJ = if_else(Spins < threshold, "AllOther", DJ)) |> 
      summarise(.by = c(AirDate,DJ),Spins=sum(Spins))
    
    pc1<-pc %>% 
      left_join(DJKey,by='DJ') %>% 
      select(AirDate,Spins,ShowName) %>%
      mutate(ShowName=if_else(is.na(ShowName),"AllOther",ShowName)) |> 
      arrange(AirDate)
    
    return(pc1)
  })
  
  top_artists_for_song<-memoise(function(song="Help",years_range=c(2010,2019)){
    years_range <- ytd(years_range)
    ts<-playlists %>% 
      filter(AirDate>=years_range[1]) %>%  
      filter(AirDate<=years_range[2]) %>%  
      filter(Title %in% song) %>%
      summarise(.by = c(ArtistToken),count=n()) %>% 
      arrange(desc(count))
    return(ts)
  })
  # ------------------ stuff for playlists tab --------------
  get_playlists<-memoise(function(show= "Diane's Kamikaze Fun Machine",
                                  date_range=c(as.Date("2017-01-02"),as.Date("2017-02-01"))){
    subset_playlists<-DJKey %>% 
      filter(ShowName %in% show) %>% 
      select(DJ) %>% 
      left_join(playlists,by = "DJ") %>% 
      select(-ArtistToken) |> 
      filter(AirDate>=date_range[1]) %>% 
      filter(AirDate<=date_range[2]) %>%
      select(-DJ)
    # print(date_range) # DEBUG
    if (nrow(subset_playlists)==0) subset_playlists <- data.frame(Title="No shows in this date range.")
    return(subset_playlists)
  })
  # ---------------OUTPUT SECTION --------------------
  # ------------------- station tab ----------------
  output$cloud <- renderWordcloud2({
    top_artists <-top_artists_reactive()
    wordcloud2a(top_artists,
                size=0.3,
                backgroundColor = "black",
                color = 'random-light',
                ellipticity = 1)
  })
  
  output$table_artists <- renderTable({
    head(top_artists_reactive(),25)
  })
  output$table_songs <- renderTable({
    top_songs_reactive()$songs
  })
  output$play_count <- renderText({
    paste("Songs Played: ",format(top_songs_reactive()$count,big.mark = ","))
  })
  output$play_count_2 <- renderText({
    paste("Songs Played: ")
  })
  # ------------------- DJs tab --------------------
  output$DJ_date_slider <- renderUI({
    dj <- filter(DJKey,ShowName==input$show_selection)
    sliderInput("DJ_years_range",
                "Year Range:",
                max = year(dj$LastShow),
                min =  year(dj$FirstShow),
                sep = "",
                step=1,
                round = TRUE,
                value = c(year(dj$FirstShow),
                          year(dj$LastShow))
    )
  })

  top_artists_DJ_reactive<-reactive({
    withProgress({
      setProgress(message = "Processing Artists...")
      DJ<-filter(DJKey,ShowName==input$show_selection) %>% pull(DJ)
      if (is.null(input$DJ_years_range)) {
        years_range<-c(1982,year(Sys.Date()))
      } else{
        # bug? when slider is on server side round option doesn't work
        years_range <- c(round(input$DJ_years_range[1]),round(input$DJ_years_range[2]))
        
      }
      ret_val<-get_top_artists_DJ(DJ,years_range)
    })
    return(ret_val)
  })
  top_songs_DJ_reactive<-reactive({
    withProgress({
      setProgress(message = "Processing Songs...")
      DJ<-filter(DJKey,ShowName==input$show_selection) %>% pull(DJ)
      if (is.null(input$DJ_years_range)) {
        years_range<-c(1982,year(Sys.Date()))
      } else{
        years_range <- c(round(input$DJ_years_range[1]),round(input$DJ_years_range[2]))
      }
      ret_val<-get_top_songs_DJ(DJ,years_range)
    })
    return(ret_val)
  })
  
  output$DJ_cloud <- renderWordcloud2({
    top_artists<-top_artists_DJ_reactive() 
    wordcloud2a(top_artists,
                size=0.3,
                backgroundColor = "black",
                color = 'random-light',
                ellipticity = 1)
  })
  output$DJ_table_distinct_artists <- renderTable({
    djDistinctive |> 
      filter(DJ==filter(DJKey,ShowName==input$show_selection) %>% pull(DJ)) |> 
      select(-DJ) |> 
      head(25)
  })
  output$DJ_table_artists <- renderTable({
    top_artists_DJ_reactive()
  })
  output$DJ_table_songs <- renderTable({
    top_songs_DJ_reactive()
  })
  
  output$DJ_table_similar <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_2) %>% 
      pull(DJ)
    ret_val <- get_similar_DJs(dj1)
    # make self-similarity 100%
    ret_val[11,5] <- "100%"
    ret_val
  })
  output$DJ_chord <- renderPlot({
    dj1<-filter(DJKey,ShowName==input$show_selection_2) %>% 
      pull(DJ)
    # get similar djs but remove target dj or matrix stuff will break
    sim_DJs<-get_similar_DJs(dj1) %>% filter(DJ!=dj1) %>% pull(DJ)
    dj_mat<-dj_mat<-as.matrix(djdtm[c(sim_DJs,dj1),])
    adj_mat1 = dj_mat %*% t(dj_mat)
    # set zeros in diagonal
    diag(adj_mat1) = 0
    #change from dJ to show name
    #dimnames(adj_mat1)<-rep(list(Docs=filter(DJKey,DJ  %in% row.names(adj_mat1)) %>% pull(ShowName)),2)
    # create graph from adjacency matrix
    graph_artists1 = graph.adjacency(adj_mat1, mode="undirected", weighted=TRUE, diag=FALSE)
    # get edgelist 1
    edges1 = get.edgelist(graph_artists1)
    
    # arc widths based on graph_artists1
    w1 = E(graph_artists1)$weight
    lwds = w1/20000
    #chord diagrams
    cdf<-bind_cols(as_tibble(edges1,.name_repair = "unique"),value=lwds)
    colset<-RColorBrewer::brewer.pal(11,'Paired')
    par(mar = rep(0, 4), bg = "black",fg="white")
    chordDiagram(cdf,annotationTrack = c('grid','name'),grid.col = colset)
    text(1,1,labels=HOST_URL)
    
  },bg="black")
  
  output$DJ_plot_sim_index <- renderPlot({
    dj1<-filter(DJKey,ShowName==input$show_selection_1DJ) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    gg<-ggplot()+
      geom_histogram(data=as_tibble(djSimilarity),aes(Similarity),color="red",bins=30)+
      geom_vline(xintercept = get_sim_index(dj1,dj2),color='lightblue',linewidth=2)
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+scale_x_continuous()
    gg<-gg+ theme(plot.background = element_rect(fill="black"))
    gg
  },bg="black")
  
  output$DJ_table_common_songs <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_1DJ) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    songs_in_common(dj1,dj2)
  })
  output$DJ_table_common_artists <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_1DJ) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    artists_in_common(dj1,dj2)
  })
  
  
  # ------------------- artist tab -------------------------------------------
  #---------------------- single artist tab-----------------------------------
  reactive_artists_1DJ<-reactive({
    input$artist_update_1DJ
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-playlists %>%
          filter(grepl(str_to_title(input$artist_letters),ArtistToken)) %>% 
          select(ArtistToken) %>%
          distinct() %>%
          arrange(ArtistToken) %>%
          pull(ArtistToken)
      })
    })
    return(ret_val)
  })
  updateSelectizeInput( session = session,
                        inputId = "artist_selection_1DJ", 
                        choices = all_artisttokens, 
                        server = TRUE,
                        selected=default_artist
  )
  process_artists_1DJ<-function(){
    withProgress({
      setProgress(message = "Processing...")
      ret_val<-play_count_by_DJ(input$artist_selection_1DJ,
                                input$artist_years_range_1DJ,
                                as.numeric(input$artist_all_other_1DJ))
    })
    return(ret_val)
  }
  
  output$artist_history_plot_1DJ <- renderPlot({
     artist_history<-process_artists_1DJ()
     # artist_history <- play_count_by_DJ("Don Felder")
    
    gg<-artist_history %>% 
      ggplot(aes(AirDate,Spins,fill=ShowName)) + 
      geom_col(orientation = "x")
      scale_x_yearqtr(format = "%Y",guide = guide_axis(check.overlap = TRUE))
    gg <- gg + labs(title=paste("Number of",input$artist_selection_1DJ,"plays every quarter by DJ"),
                    x = "Date",
                    caption=HOST_URL)
    gg<-gg + theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg + theme(plot.background = element_rect(fill="black"))
    gg
  },bg="black")
  output$top_songs_for_artist_1DJ<-renderTable({
    top_songs_for_artist(input$artist_selection_1DJ,input$artist_years_range_1DJ)
  })
  output$artist_variants<-renderTable({
    playlists %>% 
      filter(ArtistToken %in% input$artist_selection_1DJ) %>% 
      select(Artist) %>% 
      distinct()
  })
  #---------------------- multi artist tab -----------------------
  reactive_multi_artists<-reactive({
    input$artist_selection_multi
    input$artist_years_range_multi
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        #multi_tokens<-word(input$multi_artists,1:50) %>% na.omit() %>% as.character()
        ret_val<-play_count_by_artist(input$artist_selection_multi,
                                      input$artist_years_range_multi)
      })
    })
    return(ret_val)
  })
  updateSelectizeInput( session=session,
                        inputId = "artist_selection_multi", 
                        choices = all_artisttokens, 
                        server = TRUE,
                        selected=default_artist_multi
  )
  
  output$artist_variants_multi<-renderTable({
    playlists %>% 
      filter(ArtistToken %in% input$artist_selection_multi) %>% 
      select(Artist) %>% 
      distinct()
  })
  
  output$debug_multi<-renderPrint({
    input$artist_selection_multi
  })
  
  output$multi_artist_history_plot <- renderPlot({bg="black"
  multi_artist_history<-reactive_multi_artists()
  gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+geom_col()
  gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
  gg<- gg+ theme_economist()
  #gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
  gg<-gg+scale_x_continuous()
  #gg<-gg+ theme(plot.background = element_rect(fill="black"))
  gg
  },bg="black")
  
  output$multi_artist_history_plot_2 <- renderPlot({
    multi_artist_history<-reactive_multi_artists()
    gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+geom_col()
    gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
    #gg<- gg+ theme_economist()
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")    
    gg<-gg+ theme(plot.background = element_rect(fill="black"),legend.background = element_rect(fill="black"))
    gg<-gg+scale_x_continuous()
    gg<-gg+theme(legend.position = "top")
    gg
  },bg="black")
  
  output$multi_artist_history_plot_3 <- renderPlot({
    multi_artist_history<-reactive_multi_artists()
    gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+geom_col(position='dodge',width=0.4)
    gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
    #gg<- gg+ theme_economist()
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+ theme(plot.background = element_rect(fill="black"))
    gg<-gg+scale_x_continuous()
    gg
  },bg="black")
  
  output$multi_artist_history_plot_4 <- renderPlot({
    multi_artist_history<-reactive_multi_artists()
    gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+ geom_col()
    gg<-gg+ facet_grid(~ArtistToken)
    gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
    #gg<- gg+ theme_economist()
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+ theme(plot.background = element_rect(fill="black"))
    gg<-gg+scale_x_continuous() + theme(legend.position = "none")
    
    gg
  },bg="black")
  
  # ------------------ SONG TAB -----------------
  reactive_songs_letters<-reactive({
    input$song_update_1
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-playlists %>%
          filter(str_detect(Title,str_to_title(!!input$song_letters))) %>% 
          select(Title) %>%
          distinct() %>%
          arrange(Title)
      })
    })
    return(ret_val)
  })
  
  
  process_songs<-function(){
    withProgress({
      setProgress(message = "Processing...")
      ret_val<-song_play_count_by_DJ(input$song_selection,
                                     input$song_years_range,
                                     as.numeric(input$song_all_other))
    })
    return(ret_val)
  }
  
  output$SelectSong<-renderUI({
    song_choices<-reactive_songs_letters()
    selectizeInput("song_selection", h5("Select song"),
                   selected = "Help",
                   choices = song_choices,
                   multiple = TRUE
    )
  })
  output$song_history_plot <- renderPlot({
    song_history<-process_songs()
    gg<-song_history %>% 
      ggplot(aes(x=AirDate,y=Spins,fill=ShowName)) + 
      geom_col(orientation = "x")
      scale_x_yearqtr(format = "%Y",guide = guide_axis(check.overlap = TRUE))
    gg<-gg+labs(title=paste("Number of",input$song_selection,"plays every quarter by DJ"),
                x = "",
                caption=HOST_URL)
    gg<-gg+ theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+ theme(plot.background = element_rect(fill="black"),legend.background = element_rect(fill="black"))
    gg
  },bg="black")
  output$top_artists_for_song<-renderTable({
    top_artists_for_song(input$song_selection,input$song_years_range)
  })
  
  # ------------------- playlists TAB--------------------
  observeEvent(input$reset_playlist_date_range,{
    ss5<-filter(DJKey,ShowName==input$show_selection_5)
    updateDateRangeInput(session=session,
                         inputId = "playlist_date_range",
                         start =  pull(ss5,FirstShow),
                         end = pull(ss5,LastShow),
                         # min = ss5 %>% pull(FirstShow),
                         # max = ss5 %>% pull(LastShow)
    )
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
  output$playlist_table<-DT::renderDataTable({
    datatable(get_playlists(input$show_selection_5,input$playlist_date_range),
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
# -------------- CREATE SHINY APP  -----------------------------------------------------------
shinyApp(ui, server)
