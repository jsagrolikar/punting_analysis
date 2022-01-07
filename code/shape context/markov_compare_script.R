setwd("C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022/Output2")

cluster_df <- read.csv("Output1iteration.csv")
puntsinsample <- as.vector(as.character(unlist(cluster_df)))
puntsinsample <- puntsinsample[puntsinsample!="0"]
punts_to_compare <- c(puntsinsample)
iteration_df <- data.frame()
frames_to_compare <- c(1, 5)

# blocked_punt_event_df <- tracking_sample[tracking_sample$event=="punt_blocked",]
# punt_block_event_info_df <- unique(data.frame(gameId=blocked_punt_event_df$gameId, playId=blocked_punt_event_df$playId))
# punt_block_denotation <- paste(punt_block_event_info_df$gameId, "/", punt_block_event_info_df$playId, sep="")
# 
# blocked_punts <- data.frame("identity"=as.vector(punt_block_denotation))
# blocked_punts$cluster <- 0
list_to_return <- list()
markov_transition_prob_matrix_fun <- function(frames_to_compare, punts_to_compare, init_dim, curr_dim) {
  for (n in frames_to_compare) {
    test_df <- read.csv(paste("Output", n, "iteration.csv", sep=""))
    for (i in punts_to_compare) {
      index <- which(test_df==i)
      if (length(index)==0) {
        iteration_df[which(frames_to_compare==n),which(punts_to_compare==i)] <- NA
        next
      }
      column_number <- ceiling(index/nrow(test_df))
      # blocked_punts$cluster[which(punt_block_denotation==i)[1]] <- column_number
      iteration_df[which(frames_to_compare==n),which(punts_to_compare==i)] <- column_number
      print(i)
    }
    colnames(iteration_df) <- punts_to_compare
    
  }
  
  ###MARKOV CHAIN
  for (frame_id in 2:nrow(iteration_df)) {
    initial_state <- iteration_df[frame_id-1,]
    current_state <- iteration_df[frame_id,]
    init_nclusters <- unique(as.numeric(c(initial_state)))
    curr_nclusters <- unique(as.numeric(c(current_state)))
    
    if (is.na(init_dim)==FALSE) {
      init_nclusters <- c(1:init_dim)
      curr_nclusters <- c(1:curr_dim)
    }
    transition_prob_mat <- matrix(nrow=length(init_nclusters), ncol=length(curr_nclusters))
    for (cluster_id in c(na.omit(init_nclusters))) {
      fill_vec <- rep(0,length(curr_nclusters))
      cluster_transition_vec <- as.numeric(c(current_state[,(which(initial_state==cluster_id))]))
      cluster_transition_vec <- cluster_transition_vec[!is.na(cluster_transition_vec)]
      markov_transition_probabilities <- (table(cluster_transition_vec)/length(cluster_transition_vec))
      fill_vec[unique(cluster_transition_vec)] <- markov_transition_probabilities
      
      # fill_vec[which(current_state==cluster_transition_vec)] <- markov_transition_probabilities
      
      transition_prob_mat[cluster_id,] <- fill_vec
      transition_prob_mat[is.na(transition_prob_mat)] <- 0
      
      # transition_prob_mat[which(initial_state==cluster_id),] <- fill_vec
    }
  }
  # list_to_return[[1]] <- transition_prob_mat
  # list_to_return[[2]] <- iteration_df
  return(transition_prob_mat)
}
general_markov_transition_prob_matrix <- markov_transition_prob_matrix_fun(frames_to_compare, punts_to_compare, NA, NA)

###take difference in transition probability matrices of general and specific event

markers <- c("blocked_events","downed_events","touchback_events","fair_catch_events","return_class1", "return_class2","return_class3","return_class4")
markers <- list(punt_blocked_events_index,punt_downed_events_index,punt_touchback_events_index,punt_fair_catch_events_index, 
                punt_return_class1_index,punt_return_class2_index,punt_return_class3_index,punt_return_class4_index)

n <- 1
weighed_mat_list <- list()
discrepancies <- list()
for (event in markers) {
  index_df <- event
  punts_in_event <- (paste(index_df$gameId, "/", index_df$playId, sep=""))
  compare_matrix <- markov_transition_prob_matrix_fun(frames_to_compare, paste(index_df$gameId, "/", index_df$playId, sep=""), nrow(general_markov_transition_prob_matrix), ncol(general_markov_transition_prob_matrix))
  diff_matrix <- compare_matrix-general_markov_transition_prob_matrix
  cluster_count <- sapply(c(1:ncol(cluster_df)), function(x) length((cluster_df[cluster_df[,x]!=0,x])))
  weighed_mat <- sweep(diff_matrix, MARGIN=2, cluster_count, `*`)
  
  event_cluster_count <- rep(0,ncol(general_markov_transition_prob_matrix))
  event_cluster_count[ceiling(which(unlist(cluster_df) %in% punts_in_event)/nrow(cluster_df))] <- (as.vector(table(ceiling(which(unlist(cluster_df) %in% punts_in_event)/nrow(cluster_df)))))
  weighed_mat <- sweep(weighed_mat, MARGIN=2, event_cluster_count, `*`)
  weighed_mat_list[[n]] <- weighed_mat
  highlighted_transitions <- which(abs(weighed_mat)%in%head(sort(abs(weighed_mat), decreasing=TRUE)))
  column_location <- ceiling(highlighted_transitions/nrow(weighed_mat))
  row_location <- highlighted_transitions-(column_location-1)*nrow(weighed_mat)
  feature_df <- data.frame(initial_cluster=row_location, current_cluster=column_location, value=weighed_mat[highlighted_transitions])
  feature_df <- feature_df[order(-abs(feature_df$value)),]
  feature_df$event_id <- n
  
  ov_t_p <- c()
  ev_t_p <- c()
  j <- 1
  for (row_index in feature_df$initial_cluster) {
    ov_t_p[j] <- (general_markov_transition_prob_matrix[row_index,feature_df$current_cluster[j]])
    ev_t_p[j] <- (compare_matrix[row_index,feature_df$current_cluster[j]])
    j <- j+1
  }
  feature_df$overall_transition_probability <- ov_t_p
  feature_df$event_transition_probability <- ev_t_p
  
  discrepancies[[n]] <- feature_df
  # ceiling(highlighted_transitions/ncol(weighed_mat))
  
  weighed_mat[which(abs(weighed_mat)%in%head(sort(abs(weighed_mat), decreasing=TRUE)))]
  n <- n+1
}
library(formattable)
punts_downed_markov <- discrepancies[[2]]
punts_downed_markov$value <- NULL
punts_downed_markov$event_id <- NULL
punts_downed_markov$punt_outcome <- "punt_downed"
punts_downed_markov$'Observed States' <- "Frame1 : Frame5"
punts_downed_markov$TransitionLikelihood <- c("Less Likely", "Less Likely", "Less Likely", "More Likely", "Less Likely", "Less Likely")
row.names(punts_downed_markov) <- NULL
punts_downed_markov[,c(6,5,1,2,3,4,7)]
formattable(punts_downed_markov)

general_markov_transition_prob_matrix[]
markov_mat1 <- 

