library(shiny)
library(memoise)
library(wordcloud2)
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(igraph)
library(circlize)
library(xts)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tm)
library(DT)

load('DJKey.RData')
load("playlists.Rdata")
load('djSimilarity.RData')
load('djdtm.RData')


#playlists <- playlists %>% mutate_if(is.character,str_squish)

default_song<-"Help"
default_artist<-'Abba'
default_artist_multi<-c('Abba','Beatles')
max_year<-max(year(playlists$AirDate))
min_year<-min(year(playlists$AirDate))

#limit DJ list to DJs that are present in playlist file
DJKey<-DJKey %>% 
  mutate(DJ=as.character(DJ)) %>% 
  semi_join(playlists,by='DJ') %>% 
  arrange(ShowName) %>% 
  unique()

#get unique artists
all_artisttokens<-playlists %>%
  ungroup() %>% 
  select(ArtistToken) %>%
  unique() %>%
  arrange(ArtistToken) %>% 
  pull(ArtistToken)

#add artist with song to get unique songs
playlists<-playlists %>% 
  ungroup() %>% 
  mutate(artist_song=paste(ArtistToken," - ",Title))

# Define UI for dataset viewer app ----
ui <- navbarPage("WFMU Playlist Explorer BETA VERSION",theme = shinytheme("darkly"),
                 # -- Add Tracking JS File 
                 #rest of UI doesn't initiate unless tab is clicked on if the code below runs
                 #tags$head(includeScript("google-analytics.js")),
                 
                 # --------- Station/ ----------------------------------
                 tabPanel("Station",
                          titlePanel("Top Artists and Songs Played on WFMU"),fluidPage(
                            
                            
                            # ---- Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                
                                # Input: Select a dataset ----
                                selectInput("selection", "Are the DJs On Current Schedule?:",
                                            choices = c('ALL','YES','NO'),
                                            selectize = TRUE,
                                            selected = "ALL"),
                                
                                # Include clarifying text ----
                                helpText("Note: while the data view will show only the specified",
                                         "number of observations, the summary will still be based",
                                         "on the full dataset."),
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
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output: Header + summary of distribution ----
                                h4("Summary"),
                                verbatimTextOutput("summary"),
                                
                                # Output: Header + table of distribution ----
                                h4("Artists"),
                                tableOutput("table_artists"),
                                h4("Songs"),
                                tableOutput("table_songs")
                              )
                              
                            )
                          )
                 )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  # ----------------- STUFF FOR STATION TAB -----------------------------
  get_top_artists<-memoise(function(onAir="ALL",years_range = c(2010,2012)) {
    years_range <- c(round(years_range[1]),round(years_range[2]))
    if (onAir=='ALL') {
      DJ_set <-DJKey %>% 
        select(DJ)
    } else {
      DJ_set <-DJKey %>% 
        filter(onSched==onAir) %>% #on Sched or off?
        select(DJ) 
      
    }
    top_artists<-DJ_set %>% 
      left_join(playlists,by='DJ') %>%
      ungroup() %>% 
      filter(ArtistToken != "Unknown") %>% 
      filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
      filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
      group_by(ArtistToken)%>%
      summarize(play_count=n())%>%
      arrange(desc(play_count)) %>% 
      filter(ArtistToken != "") %>% 
      head(100) %>% 
      {.}
    return(top_artists)
  })
  
  get_top_songs<-memoise(function(onAir='ALL',years_range = c(2017,2020)) {
    years_range <- c(round(years_range[1]),round(years_range[2]))
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
      ungroup() %>% 
      filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
      filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
      filter(Title != "") %>% 
      group_by(artist_song)%>%
      summarize(play_count=n())%>%
      arrange(desc(play_count)) %>% 
      {.}
    top_songs <- list(count = nrow(songs),
                      songs = head(songs,25)) 
    return(top_songs)
  })
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  top_artists_reactive<-eventReactive(
    input$update,
    {
      isolate({
        withProgress({
          setProgress(message = "Processing Artists...")
          ret_val <- get_top_artists(input$selection,input$years_range_1)
        })
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
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$table_artists <- renderTable({
    head(top_artists_reactive(),25)
  })
  output$table_songs <- renderTable({
    top_songs_reactive()$songs
  })
  output$play_count <- renderText({
    paste("Songs Played: ",format(top_songs_reactive()$count,big.mark = ","))
  })
}

# Create Shiny app ----
shinyApp(ui, server)
