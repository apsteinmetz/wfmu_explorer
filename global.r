library(rmarkdown)
library(lubridate)
library(dplyr)
#library(data.table)

#setwd(paste0(getwd(),"/wfmu_explorer"))

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
  mutate(artist_song=paste(ArtistToken,Title))
library(shiny)
library(memoise)
library(wordcloud2)
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
library(tm)
library(DT)


HOST_URL<- "wfmu.servebeer.com"


## ----------------- FUNCTIONS FOR SERVER --------------------------------------
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

get_top_songs<-memoise(function(onAir='ALL',years_range = c(2010,2012)) {
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
# ----------------- STUFF FOR DJ TAB -----------------------------
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
# ------------------ stuff for playlists tab --------------
get_playlists<-memoise(function(show,date_range){
  date_range=c(as.Date("2017-01-01"),as.Date("2017-02-01"))
  subset_playlists<-DJKey %>% 
    filter(ShowName %in% show) %>% 
    select(DJ) %>% 
    left_join(playlists) %>% 
    filter(AirDate>=date_range[1]) %>% 
    filter(AirDate<=date_range[2]) %>%
    select(-artist_song,-DJ) %>% 
    as_tibble()
  if (nrow(subset_playlists)==0) subset_playlists<- data.frame(Title="No shows in this date range.")
  return(subset_playlists)
})

