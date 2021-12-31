cluster1_compare <- "2020112212/241"
cluster2_compare <- "2020122003/881"
punts_to_compare <- c(cluster1_compare, cluster2_compare)

iteration_df <- data.frame()
frames_to_compare <- c(8,9,10,11,12)
for (n in frames_to_compare) {
  test_df <- read.csv(paste(n, "iteration.csv", sep=""))
  for (i in punts_to_compare) {
    index <- which(test_df==i)
    column_number <- ceiling(index/nrow(test_df))
    blocked_punts$cluster[which(punt_block_denotation==i)[1]] <- column_number
    
    iteration_df[which(frames_to_compare==n),which(punts_to_compare==i)] <- column_number
  }
  colnames(iteration_df) <- punts_to_compare
}