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
