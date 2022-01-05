setwd("C:/Users/Jay Sagrolikar/punting_analysis/sample jaccard/")

puntsinsample <- as.vector(as.character(unlist(read.csv("1iteration.csv"))))
puntsinsample <- puntsinsample[puntsinsample!="0"]
punts_to_compare <- c(puntsinsample)
iteration_df <- data.frame()
frames_to_compare <- c(1, 2)

blocked_punt_event_df <- tracking_sample[tracking_sample$event=="punt_blocked",]
punt_block_event_info_df <- unique(data.frame(gameId=blocked_punt_event_df$gameId, playId=blocked_punt_event_df$playId))
punt_block_denotation <- paste(punt_block_event_info_df$gameId, "/", punt_block_event_info_df$playId, sep="")

blocked_punts <- data.frame("identity"=as.vector(punt_block_denotation))
blocked_punts$cluster <- 0
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

###MARKOV CHAIN
for (frame_id in 2:nrow(iteration_df)) {
  initial_state <- iteration_df[frame_id-1,]
  current_state <- iteration_df[frame_id,]
  init_nclusters <- unique(as.numeric(c(initial_state)))
  curr_nclusters <- unique(as.numeric(c(current_state)))
  
  transition_prob_mat <- matrix(nrow=length(init_nclusters), ncol=length(curr_nclusters))
  for (cluster_id in init_nclusters) {
    fill_vec <- rep(NA,length(curr_nclusters))
    cluster_transition_vec <- as.numeric(c(current_state[,(which(initial_state==cluster_id))]))
    markov_transition_probabilities <- (table(cluster_transition_vec)/length(cluster_transition_vec))
    fill_vec[unique(cluster_transition_vec)] <- markov_transition_probabilities

    # fill_vec[which(current_state==cluster_transition_vec)] <- markov_transition_probabilities

    transition_prob_mat[cluster_id,] <- fill_vec
    transition_prob_mat[is.na(transition_prob_mat)] <- 0
  
    # transition_prob_mat[which(initial_state==cluster_id),] <- fill_vec
  }

}


for (event in markers) {
  transition_prob_mat[cluster_id, ] <- length(iteration_df[iteration_df==cluster_id])*transition_prob_mat[cluster_id, ]
}


