wd <- "C:/Users/Jay Sagrolikar/punting_analysis/data"
setwd(wd)
tracking_sample <- read.csv("tracking2020.csv")
punt2020 <- tracking_sample[tracking_sample$event=="punt",]
punt2020_event_info_df <- unique(data.frame(playId=punt2020$playId, gameId=punt2020$gameId))
punt2020_df <- merge(punt2020_event_info_df, tracking_sample, by=c("gameId","playId"))

tracking_sample2019 <- read.csv("tracking2019.csv")
punt2019 <- tracking_sample2019[tracking_sample2019$event=="punt",]
punt2019_event_info_df <- unique(data.frame(playId=punt2019$playId, gameId=punt2019$gameId))
punt2019_df <- merge(punt2019_event_info_df, tracking_sample2019, by=c("gameId","playId"))

tracking_sample2018 <- read.csv("tracking2018.csv")
punt2018 <- tracking_sample2018[tracking_sample2018$event=="punt",]
punt2018_event_info_df <- unique(data.frame(playId=punt2018$playId, gameId=punt2018$gameId))
punt2018_df <- merge(punt2018_event_info_df, tracking_sample2018, by=c("gameId","playId"))


pun1_comb_df <- rbind(punt2020_df, punt2019_df)
punt_comb_df <- rbind(pun1_comb_df, punt2018_df)

write.csv(punt_comb_df,"C:/Users/Jay Sagrolikar/punting_analysis/data\\all_punts.csv", row.names = FALSE)
