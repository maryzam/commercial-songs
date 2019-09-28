library(httr)
library(jsonlite)

audio_features_api_endpoint <- "https://api.spotify.com/v1/audio-features/"

downloadFeatures <- function(spotifyId) 
{
  request_url <- paste0(audio_features_api_endpoint, spotifyId)
  response <- GET(request_url, add_headers("Authorization" = auth))
  status <- http_status(response)
  if (status$category != "Success") {
    errorMessage <- paste("Status: ", status, ". Request: ", request_url)
    print(errorMessage)
    return (NULL)
  } 

  response <- fromJSON(content(response, as="text"), 
                       flatten = TRUE, 
                       simplifyVector = TRUE)
  file_path <- paste0("audio_features/", spotifyId, ".json")
  write(toJSON(response), file = file_path)
  return (as.data.frame(response))
}

features_set <- data.frame(stringsAsFactors = FALSE)

for (song in commercials$SpotifyId) {
  print(paste("Loading data for ", song))
  response <- downloadFeatures(song)
  if (is.null(response)) {
    print(paste("Skip", song))
  }
  else {
    features_set <- rbind(features_set, response)
  }
  Sys.sleep(1)
}

write.csv(
  features_set,
  file = "audio_features.csv",
  row.names=TRUE,
  na="")


