library(rvest)
library(dplyr)
library(tidyverse)
library(httr)

songs <- data.frame(
                 Advertiser=character(),
                 Product=character(),
                 Artist=character(),
                 Song=character(), 
                 Year=integer(),
                 Title=character(),
                 Link=character(),
                 stringsAsFactors=FALSE) 

for (page in 1:21) {
  
  commercials <- read_html(paste0("https://www.tvadvertmusic.com/tag/us-commercials/page/", page)) %>%
    html_nodes("h2.st-loop-entry-title") %>%
    html_node("a") 
  
  page_songs <- data.frame("Title" = commercials  %>% html_text(), "Link" = commercials %>% html_attr("href"))
  
  songs <- bind_rows(songs, page_songs)
}

load_raw_info <- function(link) {
  response <- RETRY("GET", link)
  info <- content(response, as="parsed", type="text/html") %>%
    html_node(".st-post-content.st-entry.st-clr") %>%
    html_text()
  return(info)
}

songs <- songs %>% 
  rowwise() %>% 
  mutate(RawInfo = load_raw_info(Link))

# Parse info to extract Advertizer, year, singer & song

songs <- songs %>%
  rowwise() %>% 
  mutate(
    Artist = try_extract_artist(Raw),
    Song = try_extract_song(RawInfo),
    Year = try_extract_year(Title, RawInfo),
    Advertiser = try_extract_advertizer(Title))