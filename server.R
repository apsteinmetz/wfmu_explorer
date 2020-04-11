#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# ----------------- Define server logic ----------
shinyServer(function(input, output,session) {
  # ------------------ STATION TAB -----------------
  top_artists_reactive<-reactive({
    input$update
    isolate({      
      withProgress({
        setProgress(message = "Processing Artists...")
        ret_val <- get_top_artists(input$selection,input$years_range_1)
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
  output$cloud <- renderWordcloud2({
    # wordcloud2_rep <- repeatable(wordcloud2,seed=1234) # doesn't do anything
    top_artists <-top_artists_reactive() 
    wordcloud2(top_artists,
               size=0.3,
               backgroundColor = "black",
               color = 'random-light',
               ellipticity = 1)
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
  
  output$DJ_cloud <- renderWordcloud2({
    #wordcloud2_rep <- repeatable(wordcloud2,seed=1234)
    top_artists<-top_artists_process_DJ() 
    wordcloud2(top_artists,
               size=0.3,
               backgroundColor = "black",
               color = 'random-light',
               ellipticity = 1)
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
    par(mar = rep(0, 4), bg = "black",fg="white")
    chordDiagram(cdf,annotationTrack = c('grid','name'),grid.col = colset)
    text(1,1,labels=HOST_URL)
    
  },bg="black")
  
  output$DJ_plot_sim_index <- renderPlot({
    dj1<-filter(DJKey,ShowName==input$show_selection_1DJ) %>% pull(DJ)
    dj2<-filter(DJKey,ShowName==input$show_selection_4) %>% pull(DJ)
    gg<-ggplot()+
      geom_histogram(data=dj_similarity_tidy,aes(Similarity),color="red")+
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
    gg<-gg+theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+scale_x_continuous()
    gg<-gg+ theme(plot.background = element_rect(fill="black"))
    gg
  },bg="black")
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
      unique()
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
    gg<-gg+ theme_solarized_2(light = FALSE) + scale_colour_solarized("red")
    gg<-gg+ theme(plot.background = element_rect(fill="black"),legend.background = element_rect(fill="black"))
    gg<-gg+scale_x_continuous()
    gg
  },bg="black")
  output$top_artists_for_song<-renderTable({
    top_artists_for_song(input$song_selection,input$song_years_range)
  })
# ------------------- playlists TAB--------------------
  observe({
    ss5<-filter(DJKey,ShowName==input$show_selection_5)
    input$reset_playlist_date_range
    updateDateRangeInput(session=session,"playlist_date_range",
                         "Date Range:",
                         start =  ss5 %>% pull(FirstShow),
                         end = ss5 %>% pull(LastShow),
                         min = ss5 %>% pull(FirstShow),
                         max = ss5 %>% pull(LastShow)
    )
    output$playlist_table<-DT::renderDataTable({
      get_playlists(input$show_selection_5,input$playlist_date_range)
      print(get_playlists(input$show_selection_5,input$playlist_date_range)) # DEBUG
    })
    
  })

  
})
