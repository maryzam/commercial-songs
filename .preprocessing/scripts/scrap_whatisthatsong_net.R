library(httr)
library(jsonlite)
library(rvest)

commercials <- html("http://www.whatisthatsong.net/commercials/commercialsa-z.html")

commercials <- commercials %>%
                  html_nodes("table") %>%
                  .[[5]] %>%
                  html_table()

colnames(commercials) <- commercials[1,]

library(tidyverse)

commercials <- commercials %>% filter(Advertiser != Year)

drops <- c("Buy", "Watch")
commercials <- commercials[-1, !(colnames(commercials) %in% drops)]
commercials$spotifyId <- mapply(get_spotify_id,
                                title = commercials$`Song Title`,
                                artist = commercials$Artist,
                                year = commercials$Year)

output_path <- "commercial-songs.json"
commercials %>% 
  toJSON() %>%
  write_lines(output_path)