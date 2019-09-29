library(httr)
library(jsonlite)

track_api_endpoint <- "https://api.spotify.com/v1/tracks/"

getArtistGenre <- function(artist_url) {
  print(paste("Loading data for ", artist_url))
  response <- GET(artist_url, add_headers("Authorization" = auth))
  status <- http_status(response)
  if (status$category != "Success") {
    errorMessage <- paste("Status: ", status, ". Request: ", artist_url)
    print(errorMessage)
    return (NULL)
  } 
  response <- fromJSON(content(response, as="text"))
  return (as.vector(response$genres))
}

mergeArtistsGenres <- function(artists) {
  genres <- NULL
  for (artist_url in artists$href) {
    artist_genres <- getArtistGenre(artist_url)
    print(artist_genres)
    if (is.null(artist_genres)) {
      print(paste("Skip", artist_url))
      next
    }
    if (is.null(genres)) {
      genres <- artist_genres
    } else {
      intersect(genres, artist_genres)
    }
    Sys.sleep(1)
  }
  return (genres)
}

getTrackGenres <- function(spotifyId) 
{
  request_track_url <- paste0(track_api_endpoint, spotifyId)
  response <- GET(request_track_url, add_headers("Authorization" = auth))
  status <- http_status(response)
  if (status$category != "Success") {
    errorMessage <- paste("Status: ", status, ". Request: ", request_track_url)
    print(errorMessage)
    return (NULL)
  } 

  response <- fromJSON(content(response, as="text"))
  genrers <- mergeArtistsGenres(response$artists)
}

track_genres <- data.frame(spotifyId = character(), genres = list())

for (song in commercials$SpotifyId) {
  print(paste("Loading data for ", song))
  genres <- getTrackGenres(song)
  if (is.null(genres)) {
    print(paste("Skip", song))
    next
  }
  if (length(genres) == 0) {
    print(paste("Skip", song))
    next
  }
  track_genres <- rbind(track_genres, data.frame(spotifyId = song, genres = genres))
  Sys.sleep(1)
}

library(dplyr)
library(jsonlite)
track_genres %>% 
  group_by(spotifyId) %>% 
  summarise(genres = paste(genres, collapse=', ')) %>%
  toJSON() %>%
  write_json("track_genres.json")
