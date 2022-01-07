wd <- "C:/Users/Jay Sagrolikar/punting_analysis/data"
setwd(wd)

memory.limit(size=3000)

tracking_sample <- read.csv("tracking2020.csv")

tracking_sample2019 <- read.csv("tracking2019.csv")

tracking_sample2018 <- read.csv("tracking2018.csv")

punt_comb_df <- rbind(tracking_sample, tracking_sample2019)
punt_comb_df <- rbind(punt_comb_df, tracking_sample2018)

write.csv(punt_comb_df,"C:/Users/Jay Sagrolikar/punting_analysis/data/all_tracking.csv", row.names = FALSE)