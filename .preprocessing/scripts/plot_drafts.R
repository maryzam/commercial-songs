library(dplyr)
library(ggplot2)

songs <- left_join(commercial, audio_features_all, by="spotify_id") 

songs <- songs %>% filter(!is.na(danceability)) %>% filter(year > 2005.0)

songs$year <- as.factor(songs$year)

draw_feature <- function(dset, group_by, feature) {
  
  feature_plot <- ggplot(songs, aes_string(x=group_by, y=feature)) +
    stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="steelblue") +
    stat_summary(fun.y=median, geom = "smooth",aes(group=1), color="skyblue") +
    stat_summary(fun.y=median, geom="point", size=2, color="skyblue")
  
  feature_plot <- feature_plot + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  feature_plot
  
  plot_name <- paste0(group_by, '_', feature, '.png')
  ggsave(plot_name)
}

library(purrr)

combinations <- list(
  group_key =  c("year", "company"),
  feature =  c("danceability", "energy", "liveness", "valence", "tempo", "loudness","mode", "acousticness", "instrumentalness")
) %>% cross_df() 

combinations %>% 
  rowwise %>% 
  do(out=draw_feature(songs, .$group_key, .$feature))


