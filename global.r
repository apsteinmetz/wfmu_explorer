library(rmarkdown)
library(lubridate)
library(dplyr)

#setwd(paste0(getwd(),"/wfmu_explorer"))

load('DJKey.RData')
load("playlists.Rdata")
load('djSimilarity.RData')
load('djdtm.RData')

#load(file=url("https://www.dropbox.com/s/zobdwfuc3x1p2h8/playlists.Rdata?dl=1")) #playlists
#load(file=url("https://www.dropbox.com/s/are6e2jx8djvkl4/DJKey.RData?dl=1")) #DJKey

#limit DJ list to DJs that are present in playlist file
DJKey<-DJKey %>% 
  mutate(DJ=as.character(DJ)) %>% 
  semi_join(playlists,by='DJ') %>% 
  arrange(ShowName)

#add artist with song to get unique songs
playlists<-playlists %>% 
  ungroup() %>% 
  mutate(artist_song=paste(ArtistToken,Title))

#get range of show dates by DJ to limit year range slider

min_year<-playlists %>% 
  select(AirDate) %>% 
  top_n(-1) %>% 
  distinct() %>% 
  pull(AirDate) %>% 
  year()

FirstShow<-playlists %>% 
  group_by(DJ) %>% 
  select(DJ,AirDate) %>% 
  distinct() %>% 
  top_n(-1) %>% rename(FirstShow=AirDate)

LastShow<-playlists %>% 
  group_by(DJ) %>% 
  select(DJ,AirDate) %>% 
  distinct() %>% 
  top_n(1) %>% rename(LastShow=AirDate)

DJKey <- DJKey %>% 
  left_join(FirstShow,by="DJ") %>% 
  left_join(LastShow,by="DJ")

all_artisttokens<-playlists %>% 
  select(ArtistToken) %>% 
  unique() %>% 
  arrange(ArtistToken) %>% 
  pull(ArtistToken)
#cleanup
rm(LastShow,FirstShow)
