#scratchpad
library(tidyverse)
library(lubridate)
load("playlists.rdata")
wakelist<-playlists %>% 
  filter(DJ=="WA") %>%
  filter(AirDate>as.Date("2017-12-31")) %>% 
  {.}

wakelist <- wakelist %>% mutate(AirMonth = month(AirDate))

report<- wakelist %>% 
  group_by(AirMonth,ArtistToken) %>% 
  summarise(PlayCount=n()) %>% 
  top_n(10) %>% 
  arrange(AirMonth,desc(PlayCount))

ggplot(report,aes(AirMonth,PlayCount,fill=ArtistToken))+geom_col()
write.csv(report,"wakereport.csv")
