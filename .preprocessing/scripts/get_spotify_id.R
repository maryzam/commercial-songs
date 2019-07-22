library(httr)
library(jsonlite)

search_endpoint <- "https://api.spotify.com/v1/search"

get_spotify_id <- function(title, year) {
  
  query <- paste(title, paste("year", year, sep=":"))
  queryList <- list(type="track", q=query, limit=5)
  response <- GET(search_endpoint, 
                  add_headers("Authorization" = auth),
                  query=queryList)
  
  status <- http_status(response)
  if (status$category != "Success") {
    errorMessage <- paste("Status: ", status, ". Request: ", title, year, sep = " ")
    print(errorMessage)
    return("error")
  }
  
  response <- fromJSON(content(response, as="text"))
  tracks <- response$tracks$items
  tracks_total <- nrow(tracks)
  
  if (tracks_total== 0) {
    return("unknown")
  }
  
  if (tracks_total > 1) {
    message <- paste("Found more than one track! Total:", tracks_total, "tracks. Request: ", title, year, sep = " ")
    print(message)
  }
  
  return(tracks[1, 0]$id)
}