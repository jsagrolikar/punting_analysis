puntsinsample <- read.csv("puntsinsample.csv")
blocked <- read.csv("blocked sample.csv")
return1 <- read.csv("return 4 sample.csv")


  df_tocompare <- return1
  allpunts <- puntsinsample
  frames_to_compare <- c(1:25)

  tocompare <- rep(0, nrow(df_tocompare))
  for (i in 1:nrow(df_tocompare)) {
    tocompare[i] <- df_tocompare[i, ]
  }
  
  for (x in tocompare) {
    if (length(which(test_df==x))==0) {
      tocompare <- tocompare[-which(tocompare==x)]
    }
  }
  
  punts_to_compare <- tocompare
  
  iteration_df <- data.frame()
  

  puntdenotation <- punts_to_compare
  
  punts <- data.frame("identity"=as.vector(puntdenotation))
  punts$cluster <- 0
  setwd("C:/Users/Jay Sagrolikar/punting_analysis/sample jaccard")
  for (n in frames_to_compare) {
    test_df <- read.csv(paste(n, "iteration.csv", sep=""))
    for (i in punts_to_compare) {
      if (length(which(test_df==i))==0) {
        next
      } else {
        index <- which(test_df==i)
        column_number <- ceiling(index/nrow(test_df))
        punts$cluster[which(puntdenotation==i)] <- column_number
        
        iteration_df[which(frames_to_compare==n),which(punts_to_compare==i)] <- column_number
      }
    }
    colnames(iteration_df) <- punts_to_compare
  }
  
  #####
  
  tocompare <- rep(0, nrow(allpunts))
  for (i in 1:nrow(allpunts)) {
    tocompare[i] <- allpunts[i, ]
  }
  
  for (x in tocompare) {
    if (length(which(test_df==x))==0) {
      tocompare <- tocompare[-which(tocompare==x)]
    }
  }
  
  all_punts_to_compare <- tocompare
  
  all_iteration_df <- data.frame()
  
  # blocked_punt_event_df <- read.csv("blocked sample.csv")
  # punt_block_denotation <- c(blocked_punt_event_df)
  all_puntdenotation <- all_punts_to_compare
  
  all_punts <- data.frame("identity"=as.vector(all_puntdenotation))
  all_punts$cluster <- 0
  setwd("C:/Users/Jay Sagrolikar/punting_analysis/sample jaccard")
  for (n in frames_to_compare) {
    test_df <- read.csv(paste(n, "iteration.csv", sep=""))
    for (i in all_punts_to_compare) {
      if (length(which(test_df==i))==0) {
        next
      } else {
        index <- which(test_df==i)
        column_number <- ceiling(index/nrow(test_df))
        all_punts$cluster[which(all_puntdenotation==i)] <- column_number
        
        all_iteration_df[which(frames_to_compare==n),which(all_punts_to_compare==i)] <- column_number
      }
    }
    colnames(all_iteration_df) <- all_punts_to_compare
  }
  
  transition_list <- list()
  ###MARKOV CHAIN
  for (frame_id in 2:nrow(iteration_df)) {
    initial_state <- all_iteration_df[frame_id-1,]
    current_state <- all_iteration_df[frame_id,]
    init_nclusters <- unique(as.numeric(c(initial_state)))
    curr_nclusters <- unique(as.numeric(c(current_state)))
    
    test_init <- iteration_df[frame_id-1,]
    test_curr <- iteration_df[frame_id,]
    
    transition_prob_mat <- matrix(nrow=max(init_nclusters), ncol=max(curr_nclusters))
    for (cluster_id in 1:max(init_nclusters)) {
      fill_vec <- rep(0,max(curr_nclusters))
      cluster_transition_vec <- as.numeric(c(current_state[,(which(initial_state==cluster_id))]))
      markov_transition_probabilities <- as.data.frame(table(cluster_transition_vec)/length(cluster_transition_vec))
      
      if (length(which(test_init==cluster_id))==0) {
        
        fill_vec[sort(unique(cluster_transition_vec))] <- -1*markov_transition_probabilities$Freq
        
      } else {
        
        test_cluster_transition_vec <- as.numeric(c(test_curr[,(which(test_init==cluster_id))]))
        test_markov_transition_probabilities <- as.data.frame(table(test_cluster_transition_vec)/length(test_cluster_transition_vec))
        
        for (i in sort(unique(cluster_transition_vec))) {
          fill_vec[i] <- -1*markov_transition_probabilities[i, ]$Freq
        }
        for (j in unique(test_cluster_transition_vec)) {
          fill_vec[markov_transition_probabilities[which(markov_transition_probabilities$cluster_transition_vec==j)[1], 1]] <- 
            test_markov_transition_probabilities[which(test_markov_transition_probabilities$test_cluster_transition_vec==j)[1], 2] + fill_vec[markov_transition_probabilities[which(markov_transition_probabilities$cluster_transition_vec==j)[1], 1]]
        }
      }
      
      
      
      # fill_vec[which(current_state==cluster_transition_vec)] <- markov_transition_probabilities
      
      transition_prob_mat[cluster_id,] <- fill_vec *  length(which(iteration_df[frame_id,]==cluster_id)) *  length(which(all_iteration_df[frame_id,]==cluster_id))
      transition_prob_mat[is.na(transition_prob_mat)] <- 0
      
      
      # transition_prob_mat[which(initial_state==cluster_id),] <- fill_vec
    }
    transition_list <- append(transition_list, list(as.data.frame(transition_prob_mat)))
    
  }
  
  for (i in 1:length(transition_list)) {
    if (length(which(transition_list[[i]]>0))!=0) {
      print(i)
      print(which(transition_list[[i]]>0))
    }
  }


  
  
  
  # makemarkov <- function(df_tocompare, frames_to_compare) {
  #   
  #   tocompare <- rep(0, nrow(df_tocompare))
  #   for (i in 1:nrow(df_tocompare)) {
  #     tocompare[i] <- df_tocompare[i, ]
  #   }
  #   
  #   for (x in tocompare) {
  #     if (length(which(test_df==x))==0) {
  #       tocompare <- tocompare[-which(tocompare==x)]
  #     }
  #   }
  #   
  #   punts_to_compare <- tocompare
  #   
  #   iteration_df <- data.frame()
  #   
  #   # blocked_punt_event_df <- read.csv("blocked sample.csv")
  #   # punt_block_denotation <- c(blocked_punt_event_df)
  #   puntdenotation <- punts_to_compare
  #   
  #   punts <- data.frame("identity"=as.vector(puntdenotation))
  #   punts$cluster <- 0
  #   setwd("C:/Users/Jay Sagrolikar/punting_analysis/sample jaccard")
  #   for (n in frames_to_compare) {
  #     test_df <- read.csv(paste(n, "iteration.csv", sep=""))
  #     for (i in punts_to_compare) {
  #       if (length(which(test_df==i))==0) {
  #         next
  #       } else {
  #         index <- which(test_df==i)
  #         column_number <- ceiling(index/nrow(test_df))
  #         punts$cluster[which(puntdenotation==i)] <- column_number
  #         
  #         iteration_df[which(frames_to_compare==n),which(punts_to_compare==i)] <- column_number
  #       }
  #     }
  #     colnames(iteration_df) <- punts_to_compare
  #   }
  #   
  #   transition_list <- list()
  #   ###MARKOV CHAIN
  #   for (frame_id in 2:nrow(iteration_df)) {
  #     initial_state <- iteration_df[frame_id-1,]
  #     current_state <- iteration_df[frame_id,]
  #     init_nclusters <- unique(as.numeric(c(initial_state)))
  #     curr_nclusters <- unique(as.numeric(c(current_state)))
  #     
  #     transition_prob_mat <- matrix(nrow=max(init_nclusters), ncol=max(curr_nclusters))
  #     for (cluster_id in 1:max(init_nclusters)) {
  #       fill_vec <- rep(NA,max(curr_nclusters))
  #       cluster_transition_vec <- as.numeric(c(current_state[,(which(initial_state==cluster_id))]))
  #       markov_transition_probabilities <- (table(cluster_transition_vec)/length(cluster_transition_vec))
  #       fill_vec[unique(cluster_transition_vec)] <- markov_transition_probabilities
  #       
  #       # fill_vec[which(current_state==cluster_transition_vec)] <- markov_transition_probabilities
  #       
  #       transition_prob_mat[cluster_id,] <- fill_vec
  #       transition_prob_mat[is.na(transition_prob_mat)] <- 0
  #       
  #       
  #       # transition_prob_mat[which(initial_state==cluster_id),] <- fill_vec
  #     }
  #     transition_list <- append(transition_list, list(as.data.frame(transition_prob_mat)))
  #     
  #   }
  #   
  #   return(transition_list)
  #   
  # }
# all_markov <- makemarkov(puntsinsample, frames)
# blocked_markov <- makemarkov(blocked, frames)
# blockednet <- list()
# 
# for (i in 1:9) {
#   testmat <- blocked_markov[[i]] - all_markov[[i]]
#   blockednet <- append(blockednet, list(testmat))
# }


