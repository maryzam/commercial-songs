library(dplyr)
library(ggplot2)

songs <- left_join(commercial, audio_features_all, by="spotify_id") 
typeof(songs$danceability)

songs <- songs %>% filter(!is.na(danceability)) %>% filter(year > 2010.0)

songs$year <- as.factor(songs$year)

dance_plot <- ggplot(songs, aes(x=year, y=danceability)) +
            geom_violin(trim = FALSE) +
            stat_summary(fun.y=median, geom="point", size=1, color="steelblue") +
            stat_summary(fun.data=mean_sdl,  fun.args = list(mult = 1), geom="pointrange", color="lightblue")
dance_plot