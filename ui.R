
library(shiny)
library(rmarkdown)
library(lubridate)
library(dplyr)
library(shinycssloaders)
library(shinythemes)


shinyUI(
  navbarPage("WFMU Playlist Explorer BETA VERSION",theme = shinytheme("darkly"),
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
                                      max = max_year,
                                      sep = "",
                                      step=1,
                                      round= TRUE,
                                      value = c(max_year-3,max_year)),
                          actionButton("update","Update")
                          , textOutput("text_song_count")
                          
                        ),
                        
                        # Show Word Cloud
                        mainPanel(
                          fluidRow(
                            h4('Top Artists'),
                            tabsetPanel(type = "tabs",
                                        tabPanel("Word Cloud", 
                                                 withSpinner(plotOutput("cloud"))),
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
                                                   tabPanel("Word Cloud", 
                                                            withSpinner(plotOutput("DJ_cloud"))),
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
                                       h4('Artist names reduced to token of first two words.'),
                                       selectizeInput("artist_selection_1DJ", h4("Select one or more artists"),
                                                      choices = NULL,
                                                      multiple = TRUE,
                                                      options = list(closeAfterSelect=TRUE)
                                                      #selected=default_artist
                                                      #options = list(placeholder = 'select artist(s)')
                                       ),
                                       h4('Change the date range to include?'),
                                       sliderInput("artist_years_range_1DJ",
                                                   "Year Range:",
                                                   min = min_year,
                                                   max = max_year,
                                                   sep = "",
                                                   step=1,
                                                   round=TRUE,
                                                   value = c(2002,max_year)),
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
                        # --------------------- multi Artist ---------------
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
             # --------- Songs/ ----------------------------------
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
             #--------------- Playlists --------------------
             tabPanel("Playlists",
                      titlePanel("Get a Playlist"),
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            h4("Choose a Show:"),
                            selectInput("show_selection_5", "Show Name:",
                                        choices = DJKey$ShowName,
                                        selected = 'Teenage Wasteland'),
                            h4('Choose Date Range:'), #just to make some space for calendar
                            dateRangeInput("playlist_date_range", "Date Range:"),
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
                            #verbatimTextOutput("debug_date"),
                            withSpinner(dataTableOutput("playlist_table")),
                            h4()
                          )
                        )
                      )
             ),
             # --------- About/ ----------------------------------
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             )
             
  )
)
