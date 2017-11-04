#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(memoise)
library(wordcloud)
library(dplyr)
library(rmarkdown)
library(tidyverse)
library(lubridate)
library(igraph)
library(circlize)
library(xts)
library(stringr)
library(ggplot2)
library(ggthemes)

HOST_URL<- "wfmu.servebeer.com"

# ----------------- STUFF FOR STATION TAB -----------------------------
get_top_artists<-memoise(function(onAir,years_range) {
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
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    group_by(ArtistToken)%>%
    summarize(play_count=n())%>%
    top_n(100) %>% 
    arrange(desc(play_count))
  top_artists
})

get_top_songs<-memoise(function(onAir='ALL',years_range) {
  years_range <- c(round(years_range[1]),round(years_range[2]))
  if (onAir=='ALL') {
    DJ_set <-playlists
  } else {
    some_djs<-DJKey %>% 
      filter(onSched==onAir) %>% #on Sched or off?
      pull (DJ)
    DJ_set <-playlists %>% 
      filter(DJ %in% some_djs)
  }
  all_songs<-DJ_set %>% 
    ungroup() %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31")))
  
  top_songs<-list(songs=all_songs %>%  
    group_by(artist_song)%>%
    summarize(play_count=n())%>%
    top_n(25) %>% 
    arrange(desc(play_count)))
  top_songs$count<-nrow(all_songs)
  top_songs
})
# ----------------- STUFF FOR DJ TAB -----------------------------
get_top_artists_DJ<-memoise(function(dj,years_range) {
  years_range <- c(round(years_range[1]),round(years_range[2]))
  top_artists<-playlists %>%
    ungroup() %>% 
    filter(DJ==dj) %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    group_by(ArtistToken)%>%
    summarize(play_count=n())%>%
    top_n(100) %>% 
    arrange(desc(play_count))
  top_artists
})

get_top_songs_DJ<-memoise(function(dj,years_range) {
  years_range <- c(round(years_range[1]),round(years_range[2]))
  top_songs<-playlists %>% 
    ungroup() %>% 
    filter(DJ==dj) %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-31"))) %>%
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%
    group_by(artist_song)%>%
    summarize(play_count=n())%>%
    top_n(25) %>% 
    arrange(desc(play_count))
  top_songs
})

get_similar_DJs<-memoise(function(dj) {
  similar_DJs<-dj_similarity_tidy %>% 
    filter(DJ1==dj) %>% 
    arrange(desc(Similarity)) %>% 
    top_n(10) %>% 
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
    top_n(10) %>% 
    select(-sum_x,-sd_x) %>% 
    arrange(desc(FaveIndex)) %>%
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
    top_n(10) %>% 
    select(-sum_x,-sd_x) %>% 
    arrange(desc(FaveIndex)) %>% 
    select(-FaveIndex)
  songs
})

# ----------------- STUFF FOR SINGLE ARTIST TAB -----------------------------
play_count_by_DJ<-memoise(function(artist_token,years_range,threshold=3){
  years_range <- c(round(years_range[1]),round(years_range[2]))
  pc<- playlists %>% 
    ungroup() %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    mutate(DJ=as.character(DJ)) %>% 
    filter(ArtistToken %in% artist_token) %>% 
    mutate(AirDate=as.yearqtr(AirDate))  %>% 
    group_by(AirDate,DJ) %>% 
    summarise(Spins=n()) %>% 
    arrange(AirDate)
  
  pc1<- pc %>% 
    filter(Spins>=threshold)
  
  #lump together all DJ's who played the artist less than 'threshold' times
  pc2<- pc %>%
    ungroup() %>% 
    filter(Spins<threshold) %>% 
    group_by(AirDate) %>% 
    summarise(Spins=sum(Spins)) %>% 
    mutate(ShowName='AllOther')
  
  pc3<-pc1 %>% 
    left_join(DJKey,by='DJ') %>% 
    select(AirDate,Spins,ShowName) %>% 
    full_join(pc2) %>%
    ungroup()
  
  return(pc3)
})

play_count_by_artist<-memoise(function(artist_tokens,years_range=c(2012,2015)){
  years_range <- c(round(years_range[1]),round(years_range[2]))
  pc<- playlists %>% 
    ungroup() %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    filter(ArtistToken %in% artist_tokens) %>% 
    mutate(AirDate=year(AirDate))  %>% 
    group_by(AirDate,ArtistToken) %>% 
    summarise(Spins=n()) %>% 
    arrange(AirDate)
  
  return(pc)
})

top_songs_for_artist<-memoise(function(artist_token,years_range=c(2012,2015)){
  years_range <- c(round(years_range[1]),round(years_range[2]))
  ts<-playlists %>% 
    filter(ArtistToken %in% artist_token) %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    group_by(Title) %>% 
    summarise(count=n()) %>% 
    arrange(desc(count))
  return(ts)
})

# ----------------- STUFF FOR SONG TAB -----------------------------
song_play_count_by_DJ<-memoise(function(songs,years_range,threshold=3){
  years_range <- c(round(years_range[1]),round(years_range[2]))
  pc<- playlists %>% 
    ungroup() %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    mutate(DJ=as.character(DJ)) %>% 
    filter(Title %in% songs) %>% 
    mutate(AirDate=as.yearqtr(AirDate))  %>% 
    group_by(AirDate,DJ) %>% 
    summarise(Spins=n()) %>% 
    arrange(AirDate)
  
  pc1<- pc %>% 
    filter(Spins>=threshold)
  
  #lump together all DJ's who played the artist less than 'threshold' times
  pc2<- pc %>%
    ungroup() %>% 
    filter(Spins<threshold) %>% 
    group_by(AirDate) %>% 
    summarise(Spins=sum(Spins)) %>% 
    mutate(ShowName='AllOther')
  
  pc3<-pc1 %>% 
    left_join(DJKey,by='DJ') %>% 
    select(AirDate,Spins,ShowName) %>% 
    full_join(pc2) %>%
    ungroup()
  
  return(pc3)
})

top_artists_for_song<-memoise(function(song,years_range=c(2012,2015)){
  years_range <- c(round(years_range[1]),round(years_range[2]))
  ts<-playlists %>% 
    filter(Title %in% song) %>% 
    filter(AirDate>=as.Date(paste0(years_range[1],"-1-1"))) %>%  
    filter(AirDate<=as.Date(paste0(years_range[2],"-12-31"))) %>%  
    group_by(ArtistToken) %>% 
    summarise(count=n()) %>% 
    arrange(desc(count))
  return(ts)
})

# ----------------- Define server logic ----------
shinyServer(function(input, output,session) {
  # ------------------ STATION TAB -----------------
  top_artists_reactive<-reactive({
    input$update
    isolate({      
      withProgress({
        setProgress(message = "Processing Artists...")
        ret_val<-get_top_artists(input$selection,input$years_range_1)
      })
    })
    return(ret_val)
  })
  top_songs_reactive<-reactive({
    input$update
    isolate({      
      withProgress({
        setProgress(message = "Processing Songs...")
        ret_val<-get_top_songs(input$selection,input$years_range_1)
      })
    })
    return(ret_val)
  })
  
  output$most_recent_date<-renderText({
    paste(max(playlists$AirDate)) #paste needed so date formats correctly. print doesn't do it.
    })
  output$cloud <- renderPlot({
    wordcloud_rep <- repeatable(wordcloud,seed=1234)
    top_artists<-top_artists_reactive() 
    scaleFactor=2
    wordcloud_rep(words = top_artists$ArtistToken, 
                  freq = top_artists$play_count^scaleFactor,
                  max.words=100, 
                  min.freq=20,
                  random.order=FALSE, 
                  colors=brewer.pal(8, "Dark2"),
                  scale = c(4,.5),
                  #fixed.asp=FALSE,
                  rot.per=0.35)
    text(1,1,labels=HOST_URL)
  })
  output$table_artists <- renderTable({
    top_artists_reactive()
  })
  output$table_songs <- renderTable({
    top_songs_reactive()$songs
  })
  output$text_song_count <- renderText({
    paste("Plays found:",format(top_songs_reactive()$count,big.mark = ","))
  })
  # ------------------ DJ TAB -----------------
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
  
  top_artists_process_DJ<-function(){
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
  }
  top_songs_process_DJ<-function(){
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
  }
  
  output$DJ_cloud <- renderPlot({
    wordcloud_rep <- repeatable(wordcloud,seed=1234)
    top_artists<-top_artists_process_DJ() 
    scaleFactor=2
    wordcloud_rep(words = top_artists$ArtistToken, 
                  freq = top_artists$play_count^scaleFactor,
                  max.words=100, 
                  random.order=FALSE,rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"),
                  scale = c(4,.3))
    text(1,1,labels=HOST_URL)
  })
  output$DJ_table_artists <- renderTable({
    top_artists_process_DJ()
  })
  output$DJ_table_songs <- renderTable({
    top_songs_process_DJ()
  })
  
  output$DJ_table_similar <- renderTable({
    dj1<-filter(DJKey,ShowName==input$show_selection_2) %>% pull(DJ)
    get_similar_DJs(dj1)
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
    cdf<-bind_cols(as_tibble(edges1),value=lwds)
    colset<-RColorBrewer::brewer.pal(11,'Paired')
    chordDiagram(cdf,annotationTrack = c('grid','name'),grid.col = colset)
    text(1,1,labels=HOST_URL)
    
  })
  
  output$DJ_plot_sim_index <- renderPlot({
    dj1<-filter(DJKey,ShowName==input$show_selection_1DJ) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    ggplot(dj_similarity_tidy,aes(Similarity))+
      geom_density()+
      geom_vline(xintercept = get_sim_index(dj1,dj2),color='blue')
  })
  
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
  
  #------------------- SINGLE ARTIST TAB-----------------------------------
  reactive_artists_1DJ<-reactive({
    input$artist_update_1DJ
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-playlists %>%
          ungroup() %>%
          filter(grepl(str_to_title(input$artist_letters),ArtistToken)) %>% 
          select(ArtistToken) %>%
          distinct() %>%
          arrange(ArtistToken) %>%
          pull(ArtistToken)
      })
    })
    return(ret_val)
  })
  updateSelectizeInput( session=session,
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
                                input$artist_all_other_1DJ)
    })
    return(ret_val)
  }
  
  output$artist_history_plot_1DJ <- renderPlot({
    artist_history<-process_artists_1DJ()
    gg<-artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ShowName))+geom_col()
    gg<-gg+labs(title=paste("Number of",input$artist_selection_1DJ,"plays every quarter by DJ"),
                caption=HOST_URL)
    gg<-gg+scale_x_continuous()
    gg
  })
  output$top_songs_for_artist_1DJ<-renderTable({
    top_songs_for_artist(input$artist_selection_1DJ,input$artist_years_range_1DJ)
  })
  output$artist_variants<-renderTable({
    playlists %>% 
      filter(ArtistToken %in% input$artist_selection_1DJ) %>% 
      select(Artist) %>% 
      unique()
  })
  #-------------------- multi artist tab -----------------------
  reactive_multi_artists<-reactive({
    input$artist_update_2
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        multi_tokens<-word(input$multi_artists,1:50) %>% na.omit() %>% as.character()
          ret_val<-play_count_by_artist(multi_tokens,
                                    input$multi_artist_years_range)
      })
    })
    return(ret_val)
  })


  output$multi_artist_history_plot <- renderPlot({
    multi_artist_history<-reactive_multi_artists()
    gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+geom_col()
    gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
    gg<- gg+ theme_economist()
    #gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+scale_x_continuous()
    gg
  })

  output$multi_artist_history_plot_2 <- renderPlot({
    multi_artist_history<-reactive_multi_artists()
    gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+geom_col()
    gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
    #gg<- gg+ theme_economist()
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+scale_x_continuous()
    gg
  })
  
  output$multi_artist_history_plot_3 <- renderPlot({
    multi_artist_history<-reactive_multi_artists()
    gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+geom_col(position='dodge',width=0.4)
    gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
    #gg<- gg+ theme_economist()
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+scale_x_continuous()
    gg
  })
  
  output$multi_artist_history_plot_4 <- renderPlot({
    multi_artist_history<-reactive_multi_artists()
    gg<-multi_artist_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ArtistToken))+ geom_col()
    gg<-gg+ facet_grid(~ArtistToken)
    gg<-gg+labs(title=paste("Annual Plays by Artist"),caption=HOST_URL)
    #gg<- gg+ theme_economist()
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+scale_x_continuous() + theme(legend.position = "none")

    gg
  })
  
  # ------------------ SONG TAB -----------------
  reactive_songs_letters<-reactive({
    input$song_update_1
    isolate({      
      withProgress({
        setProgress(message = "Processing...")
        ret_val<-playlists %>%
          ungroup() %>%
          filter(grepl(str_to_title(input$song_letters),Title)) %>% 
          select(Title) %>%
          distinct() %>%
          arrange(Title) %>%
          pull(Title)
      })
    })
    return(ret_val)
  })
  
  
  process_songs<-function(){
    withProgress({
      setProgress(message = "Processing...")
      ret_val<-song_play_count_by_DJ(input$song_selection,
                                input$song_years_range,
                                input$song_all_other)
    })
    return(ret_val)
  }
  
  output$SelectSong<-renderUI({
    song_choices<-reactive_songs_letters()
    selectizeInput("song_selection", h5("Select song"),
                choices = song_choices,
                selected= default_song,
                multiple = TRUE
    )
  })
  output$song_history_plot <- renderPlot({
    song_history<-process_songs()
    gg<-song_history %>% ggplot(aes(x=AirDate,y=Spins,fill=ShowName))+geom_col()
    gg<-gg+labs(title=paste("Number of",input$song_selection,"plays every quarter by DJ"),
                caption=HOST_URL)
    gg<-gg+scale_x_continuous()
    gg
  })
  output$top_artists_for_song<-renderTable({
    top_artists_for_song(input$song_selection,input$song_years_range)
  })
  
})
