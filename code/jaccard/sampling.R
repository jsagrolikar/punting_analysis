og_sample <- read.csv("all_punts.csv")
tracking_sample <- og_sample[0, ]
index_list <- list(punt_blocked_events_index, punt_downed_events_index, punt_fair_catch_events_index, punt_touchback_events_index,
                   punt_return_class1_index, punt_return_class2_index, punt_return_class3_index, punt_return_class4_index)

for (i in index_list) {
  random_ids <- sample.int(nrow(i), ceiling(nrow(i)/2))
  indexplays <- i[random_ids, 2]
  indexgames <- i[random_ids, 1]
  index <- data.frame(gameId=indexgames, playId=indexplays)
  testsample <- merge(og_sample, index, by=c("gameId","playId"))
  # testsample <- og_sample[og_sample$gameId %in% indexgames & og_sample$playId %in% indexplays, ]
  tracking_sample <- rbind(tracking_sample, testsample)
}

sdir <- "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022/"
fname <- "2250sample.csv"
write.csv(tracking_sample, paste(sdir, fname, sep=""), row.names = FALSE)