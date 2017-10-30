
library(shiny)
library(rmarkdown)
library(lubridate)
library(dplyr)


shinyUI(
  navbarPage("WFMU Playlist Explorer ALPHA VERSION",
             # -- Add Tracking JS File 
             #rest of UI doesn't initiate unless tab is clicked on if the code below runs
             #tags$head(includeScript("google-analytics.js")),
             
             # --------- Station/ ----------------------------------
             tabPanel("Station",
                      titlePanel("Top Artists and Songs Played on WFMU"),
                      
                      sidebarLayout(
                        
                        # Sidebar with a slider and selection inputs
                        sidebarPanel(
                          h4("Last Playlist Update:"),
                          h4(textOutput("most_recent_date")),
                          
                          selectInput("selection", "Are the DJs On Current Schedule?:",
                                      choices = c('ALL','YES','NO')),
                          h4('Be aware a wide date range could take many seconds to process.'),
                          sliderInput("years_range_1",
                                      "Year Range:",
                                      min = min_year,
                                      max = year(Sys.Date()),
                                      sep = "",
                                      step=1,
                                      round= TRUE,
                                      value = c(year(Sys.Date())-3,year(Sys.Date()))),
                          actionButton("update","Update")
                          , textOutput("text_song_count")
                          
                        ),
                        
                        # Show Word Cloud
                        mainPanel(
                          fluidRow(
                            h4('Top Artists'),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Word Cloud", plotOutput("cloud")),
                                        tabPanel("Table", tableOutput("table_artists"))
                            )),
                          fluidRow(
                            h4('Top Songs'),
                            tableOutput("table_songs")
                          )
                        )
                      )
             ),
             # --------- DJs/ ------------------------------------
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
                                                   tabPanel("Word Cloud", plotOutput("DJ_cloud")),
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
                                       h4('Most Similar Shows Based on Common Artists'),
                                       tableOutput("DJ_table_similar")
                                       
                                     ),
                                     fluidRow(
                                       h4('DJ Neighborhood') 
                                       , plotOutput("DJ_chord")
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
                                          h5('Black curve is frequency of all DJ pair similarities. Vertical blue line is similarity of this pair.'),
                                          h5(' The bulge at the low end shows WFMU DJs are not very similar to each other, in general.')
                                   ),
                                   column(6,
                                          plotOutput("DJ_plot_sim_index",height="100px")
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
             ),
             # --------- Artists/ ----------------------------------
             navbarMenu("Artists",
                        #----------- Single Artist -----------------------
                        tabPanel("Single Artist",
                                 titlePanel("Artists Plays by DJ Over Time"),
                                 sidebarLayout(
                                   # Sidebar with a slider and selection inputs
                                   sidebarPanel(
                                     fluidRow(
                                       selectizeInput("artist_selection_1DJ", h4("Select one or more artists"),
                                                      choices = NULL,
                                                      multiple = TRUE,
                                                      options = list(placeholder = 'select artist(s)')
                                       ),
                                       h4('Change the date range to include?'),
                                       sliderInput("artist_years_range_1DJ",
                                                   "Year Range:",
                                                   min = min_year,
                                                   max = year(Sys.Date()),
                                                   sep = "",
                                                   step=1,
                                                   round=TRUE,
                                                   value = c(2002,year(Sys.Date()))),
                                       h4('Change threshold to show DJ name?'),
                                       selectInput("artist_all_other_1DJ",
                                                   "Threshold of Minimum Plays to show DJ",
                                                   selected = 3,
                                                   choices=1:9)
                                     )
                                     
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       h4('Artist Plays per Quarter'),
                                       textOutput("chosen"),
                                       plotOutput("artist_history_plot_1DJ"),
                                       h4('Songs Played of this Artist'),
                                       tableOutput('top_songs_for_artist_1DJ')
                                     )
                                   )
                                 )
                        ),
                        # --------------------- multi Artist ---------------
                        tabPanel("Multi Artist",
                                 titlePanel("Artists Plays Over Time"),
                                 sidebarLayout(
                                   # Sidebar with a slider and selection inputs
                                   sidebarPanel(
                                     h4('ArtistToken separated by spaces'),
                                     h5('Determine proper ArtistToken on Single Artist tab.'),
                                     h5('This thing has no tolerace for misspelling and is case-senstive!'),
                                     h5('Given there are nearly 40 thousand artists, a drop down list is impractical.'),
                                     textInput("multi_artists", 
                                               label = h4("Tokens Separated by Spaces"), 
                                               value = "JustinBeiber Rihanna CharlesManson"),
                                     uiOutput('SelectMultiArtists'),
                                     h4('Change the date range?'),
                                     sliderInput("multi_artist_years_range",
                                                 "Year Range:",
                                                 min = min_year,
                                                 max = year(Sys.Date()),
                                                 sep = "",
                                                 step=1,
                                                 round=TRUE,
                                                 value = c(2005,year(Sys.Date()))),
                                     actionButton("artist_update_2",
                                                  "Find Artists")
                                   ),
                                   
                                   mainPanel(
                                     fluidRow(
                                       h4('Artist Plays per Year (light version)'),
                                       plotOutput("multi_artist_history_plot",width = "710px",height="355px"),
                                       h4('Artist Plays per Year (goth style)'),
                                       plotOutput("multi_artist_history_plot_2",width = "710px",height="355px")
                                       
                                     )
                                   )
                                   
                                 )
                                 
                        )
             ),
             # --------- Songs/ ----------------------------------
             # tabPanel("Songs",
             #          titlePanel("Nothing Here Yet")
             # ),
             # --------- About/ ----------------------------------
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             )
             
  )
)
