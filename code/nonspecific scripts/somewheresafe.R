blocked_punt_event_df <- tracking_sample[tracking_sample$event=="punt_blocked",]
punt_block_event_info_df <- unique(data.frame(gameId=blocked_punt_event_df$gameId, playId=blocked_punt_event_df$playId))
punt_block_denotation <- paste(punt_block_event_info_df$gameId, "/", punt_block_event_info_df$playId, sep="")

setwd("C:/Users/Jay Sagrolikar/punting_analysis/testing")
test_df <- read.csv("12iteration.csv")

blocked_punts <- data.frame("identity"=as.vector(punt_block_denotation))
blocked_punts$cluster <- 0

for (i in punt_block_denotation) {
  index <- which(test_df==i)
  column_number <- ceiling(index/nrow(test_df))
  # colnames(test_df)[column_number]
  blocked_punts$cluster[which(punt_block_denotation==i)[1]] <- column_number
}
