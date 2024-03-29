library(httr)
library(jsonlite)

search_endpoint <- "https://api.spotify.com/v1/search"

get_spotify_id <- function(track_title, artist, year) {
  
  query <- paste(paste("track", track_title, sep = ":"),
                 paste("artist", artist, sep = ":"))
  
  if (year == 0) {
    query <- paste(query, paste("year", year, sep=":"))
  }
  
  queryList <- list(type="track", q=query, limit=5)
  response <- GET(search_endpoint, 
                  add_headers("Authorization" = auth),
                  query=queryList)
  
  status <- http_status(response)
  if (status$category != "Success") {
    errorMessage <- paste("Status: ", status, ". Request: ", paste(track_title, artist, year, sep = " / "), sep = " ")
    print(errorMessage)
    return("error")
  }
  
  response <- fromJSON(content(response, as="text"))
  tracks <- response$tracks$items
  tracks_total <- nrow(tracks)
  
  if (is.null(tracks_total) || tracks_total == 0) {
    if (year == 0) {
      return("unknown")
    }
    print(paste("lookup without year for ", track_title))
    return(get_spotify_id(track_title, artist, 0))
  }
  
  if (tracks_total > 1) {
    message <- paste("Found more than one track! Total:", tracks_total, "tracks. Request: ", track_title, year, sep = " ")
    print(message)
  }
  spotify_id <- tracks[1, ]$id
  print(spotify_id)
  return(spotify_id)
}


