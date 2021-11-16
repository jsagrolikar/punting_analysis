library(ggplot2)
library(dplyr)
library(gtools)
library(data.table)
library(cluster)
library(tidyverse)

get_dist_matrix <- function(frame_punt_events_df) {
  punt_index_df <- (unique(data.frame(gameId=frame_punt_events_df$gameId, playId=frame_punt_events_df$playId)))
  punt_index_df$punt_id <- 1:nrow(punt_index_df)
  row.names(punt_index_df) <- NULL
  
  n <- (nrow(punt_index_df)-1)
  dist_mat <- matrix(nrow=n, ncol=n)
  formation_list <- list()
  punt_cluster_assignment_mat <- matrix(nrow=n+1, ncol=n+1)
  punt_cluster_assignment_mat[1,] <- c(1:(n+1))
  for (i in 1:n) {
    spec_punt_df <- frame_punt_events_df[frame_punt_events_df$gameId==punt_index_df[i,]$gameId & frame_punt_events_df$playId==punt_index_df[i,]$playId,]
    spec_punt_df$punt_id <- i
    spec_punt_punting_df <- spec_punt_df[spec_punt_df$possessionTeam==spec_punt_df$TeamAbbr,]
    spec_punt_punting_df <- rbind(spec_punt_punting_df[spec_punt_punting_df$position=="P",],spec_punt_punting_df[spec_punt_punting_df$position!="P",])
    spec_punt_receiving_df <- spec_punt_df[spec_punt_df$possessionTeam!=spec_punt_df$TeamAbbr,]
    spec_punt_receiving_df <- rbind(spec_punt_receiving_df[spec_punt_receiving_df$x==max(spec_punt_receiving_df$x),],spec_punt_receiving_df[spec_punt_receiving_df$x!=max(spec_punt_receiving_df$x),])
    
    other_punt_df <- merge(frame_punt_events_df, punt_index_df[(i+1):(n+1),], by=c("gameId","playId"))
    if (i>=n) {
      other_punt_df <- merge(frame_punt_events_df, punt_index_df[i+1,], by=c("gameId","playId"))
    }
    other_punt_df <- other_punt_df[order(other_punt_df$y,other_punt_df$x),]
    other_punt_punting_df <- other_punt_df[other_punt_df$possessionTeam==other_punt_df$TeamAbbr,]
    # other_punt_punting_df <- other_punt_punting_df[order(other_punt_punting_df$y,other_punt_punting_df$x),]
    other_punt_punting_df <- rbind(other_punt_punting_df[other_punt_punting_df$position=="P",],other_punt_punting_df[other_punt_punting_df$position!="P",])
    other_punt_punting_df <- other_punt_punting_df[order(other_punt_punting_df$gameId,other_punt_punting_df$playId),]
    other_punt_receiving_df <- other_punt_df[other_punt_df$possessionTeam!=other_punt_df$TeamAbbr,]
    other_returners_df <- merge(aggregate(x~gameId+playId, other_punt_receiving_df, max),other_punt_receiving_df, by=c("gameId","playId","x"))
    other_punt_receiving_df <- other_punt_receiving_df[!(other_punt_receiving_df$gameId %in% other_returners_df$gameId & other_punt_receiving_df$playId %in% other_returners_df$playId & other_punt_receiving_df$nflId %in% other_returners_df$nflId),]
    other_punt_receiving_df <- rbind(other_returners_df,other_punt_receiving_df)
    # other_punt_receiving_df <- rbind(other_punt_receiving_df[other_punt_receiving_df$x==max(other_punt_receiving_df$x),],other_punt_receiving_df[other_punt_receiving_df$x!=max(other_punt_receiving_df$x),])
    other_punt_receiving_df <- other_punt_receiving_df[order(other_punt_receiving_df$gameId,other_punt_receiving_df$playId),]
    
    total_cluster_distance <- (sqrt(((spec_punt_punting_df$x-other_punt_punting_df$x)^2)+((spec_punt_punting_df$y-other_punt_punting_df$y)^2)))+
      (sqrt(((spec_punt_receiving_df$x-other_punt_receiving_df$x)^2)+((spec_punt_receiving_df$y-other_punt_receiving_df$y)^2)))
    summed_cluster_distance <- (unname(tapply(total_cluster_distance, (seq_along(total_cluster_distance)-1) %/% 11, sum)))
    formation_comp_df <- data.frame(gameId_1=unique(spec_punt_df$punt_id), gameId_2=unique(other_punt_df$punt_id), formation_distance=summed_cluster_distance)
    dist_mat[i,] <- c(rep(Inf,(i-1)), (summed_cluster_distance))
    # dist_mat[is.na(dist_mat)] <- Inf
    dist_mat <- cbind(Inf, dist_mat)
    loc_mat <- which(dist_mat==min(dist_mat), arr.ind=TRUE)
    
    existing_clusters <- c(na.omit(c(punt_cluster_assignment_mat[,loc_mat[1]], punt_cluster_assignment_mat[,loc_mat[2]])))
    
    punt_cluster_assignment_mat[,min(loc_mat)] <- c(existing_clusters, rep(NA,((n+1)-length(existing_clusters))))
    punt_cluster_assignment_mat <- punt_cluster_assignment_mat[,-max(loc_mat)]
    print(min(dist_mat))
    print(loc_mat)
    # print(punt_cluster_assignment_mat)
    
    formation_list[[i]] <- formation_comp_df

    # data.frame("kicking_team")
    # print(total_cluster_distance)
    
  }
  return(dist_mat)
  
}
get_dist_matrix(frame_punt_events_df, filt_punt_events_df)

minval <- which.min(dist_mat)
