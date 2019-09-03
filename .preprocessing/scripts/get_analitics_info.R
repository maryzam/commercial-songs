library(httr)
library(jsonlite)

analitics_api_endpoint <- "https://api.spotify.com/v1/audio-analysis/"

interesting_fields <- c('track', 'sections')

uploadAnalitics <- function(spotifyId) 
{
  request_url <- paste0(analitics_api_endpoint, spotifyId)
  response <- GET(request_url, add_headers("Authorization" = auth))
  status <- http_status(response)
  if (status$category != "Success") {
    errorMessage <- paste("Status: ", status, ". Request: ", request_url)
    print(errorMessage)
    return (NULL)
  } 
  
  response <- fromJSON(content(response, as="text"))
  full_file_path <- paste0("full/", spotifyId, ".json")
  write(toJSON(response), file = full_file_path)
  
  response <- response[names(response) %in% interesting_fields == TRUE] 
  minified_file_path <- paste0("minified/", spotifyId, ".json")
  write(toJSON(response), file = minified_file_path)
  
  return (response)
}
