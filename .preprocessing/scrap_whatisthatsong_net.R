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
commercials <- commercials[, !(colnames(commercials) %in% drops)]

output_path <- "commercial-songs.json"
commercials %>% 
  toJSON() %>%
  write_lines(output_path)