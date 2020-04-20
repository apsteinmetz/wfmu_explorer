library(shiny)
library(shinycssloaders)
library(shinythemes)
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
source("wordcloud2a.r")

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
                 # --------- DJ TAB ----------------------------------
                 navbarMenu("DJs",
                            # --------- DJs/DJ Profile -----------------------------
                            tabPanel("DJ Profile",
                                     titlePanel("DJ Profile"),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("show_selection", "Show Name:",
                                                     choices = DJKey$ShowName,
                                                     selected = 'Teenage Wasteland'),
                                         hr(),
                                         uiOutput("DJ_date_slider")
                                         #, actionButton("DJ_update","Update")
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
                                                     selected = 'Teenage Wasteland')
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
                                                          selected = 'Teenage Wasteland')
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
                 ) # end DJ tab
) # end UI

# --------------SERVER -----------------------------------------------------------
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
  
  # ----------------- FUNCTIONS FOR DJS TAB -----------------------------
  get_top_artists_DJ<-memoise(function(dj="TW",years_range = c(2017,2019)) {
    years_range <- c(round(years_range[1]),round(years_range[2]))
    top_artists<-playlists %>%
      ungroup() %>% 
      filter(DJ==dj) %>% 
      filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  
      filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
      group_by(ArtistToken)%>%
      summarize(play_count=n())%>%
      arrange(desc(play_count)) %>% 
      head(100)
    return(top_artists)
  })
  
  get_top_songs_DJ<-memoise(function(dj="TW",years_range = c(2017,2019)) {
    years_range <- c(round(years_range[1]),round(years_range[2]))
    top_songs<-playlists %>% 
      ungroup() %>% 
      filter(DJ==dj) %>% 
      filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%
      filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%
      group_by(artist_song)%>%
      summarize(play_count=n())%>%
      arrange(desc(play_count)) %>% 
      head(25)
    return(top_songs)
  })
  
  get_similar_DJs<-memoise(function(dj) {
    similar_DJs<-dj_similarity_tidy %>% 
      filter(DJ1==dj) %>% 
      arrange(desc(Similarity)) %>% 
      head(10) %>% 
      ungroup() %>% 
      rename(DJ=DJ2) %>% 
      select(DJ,Similarity) %>% 
      left_join(DJKey,by='DJ') %>%
      mutate(Similarity=paste0(trunc(Similarity*100),"%")) %>% 
      #add target dj to top of table so we see the 2-letter code for the chord chart
      full_join(filter(DJKey,DJ==dj)) %>% 
      select(ShowName,DJ,onSched,showCount,Similarity) 
    
    similar_DJs
  })
  
  get_sim_index<-memoise(function(dj1,dj2) {
    DJ_sim<-dj_similarity_tidy %>% 
      filter(DJ1==dj1,DJ2==dj2) %>%
      pull(Similarity)
    DJ_sim
  })
  
  artists_in_common<-memoise(function(dj1,dj2){
    artists<-playlists %>% 
      filter(DJ %in% c(dj1,dj2)) %>% 
      group_by(DJ,ArtistToken) %>% 
      summarise(n=n()) %>%
      spread(DJ,n) %>% 
      mutate(sum_x=rowSums(.[2:ncol(.)],na.rm=TRUE)) %>% 
      mutate(sd_x=.[2:ncol(.)] %>% na.fill(0) %>% apply(1,sd)) %>% 
      mutate(FaveIndex=trunc(sum_x-1.8*sd_x)) %>% 
      #select(ArtistToken,sum,FaveIndex) %>% 
      arrange(desc(FaveIndex)) %>%
      head(10) %>% 
      select(-sum_x,-sd_x) %>% 
      select(-FaveIndex)
    artists
  })
  
  songs_in_common<-memoise(function(dj1,dj2){
    songs<-playlists %>% 
      filter(DJ %in% c(dj1,dj2)) %>%
      mutate(Artist_Title=paste(Artist,Title)) %>% 
      group_by(DJ,Artist_Title) %>% 
      summarise(n=n()) %>%
      spread(DJ,n) %>% 
      mutate(sum_x=rowSums(.[2:ncol(.)],na.rm=TRUE)) %>% 
      mutate(sd_x=.[2:ncol(.)] %>% na.fill(0) %>% apply(1,sd)) %>% 
      mutate(FaveIndex=trunc((sum_x-1.8*sd_x)*10)) %>% #apply some arbitrary scaling
      #select(ArtistToken,sum,FaveIndex) %>% 
      select(-sum_x,-sd_x) %>% 
      arrange(desc(FaveIndex)) %>% 
      select(-FaveIndex) %>% 
      head(10) %>% 
      
      {.}
    songs
  })
  # ----------------- OUTPUT SECTION --------------------
  # station tab
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
  # DJs tab
  output$DJ_date_slider <- renderUI({
    sliderInput("DJ_years_range",
                "Year Range:",
                min = filter(DJKey,ShowName==input$show_selection) %>% pull(FirstShow) %>% year(),
                max = filter(DJKey,ShowName==input$show_selection) %>% pull(LastShow) %>% year(),
                sep = "",
                step=1,
                round = TRUE,
                value = c(min,max)
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
  output$DJ_table_artists <- renderTable({
    top_artists_DJ_reactive()
  })
  output$DJ_table_songs <- renderTable({
    top_songs_DJ_reactive()
  })
  
  output$DJ_table_similar <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_2) %>% pull(DJ)
    ret_val <- get_similar_DJs(dj1)
    # make self-similarity 100%
    ret_val[11,5] <- "100%"
    ret_val
  })
  output$DJ_chord <- renderPlot({
    dj1<-filter(DJKey,ShowName==input$show_selection_2) %>% pull(DJ)
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
      geom_histogram(data=dj_similarity_tidy,aes(Similarity),color="red",bins=30)+
      geom_vline(xintercept = get_sim_index(dj1,dj2),color='lightblue',size=2)
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
  
}
# -------------- CREATE SHINY APP  -----------------------------------------------------------
shinyApp(ui, server)
