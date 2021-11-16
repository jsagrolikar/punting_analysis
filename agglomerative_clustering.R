library(ggplot2)
library(dplyr)
library(gtools)
library(data.table)
library(cluster)
library(tidyverse)

n <- (nrow(frame_punt_events_df))^(1/2) - 1
punts_lists <- list()
for (i in 1:n) {
  spec_punt_df <- frame_punt_events_df[frame_punt_events_df$gameId==punt_index_df[i,]$gameId & frame_punt_events_df$playId==punt_index_df[i,]$playId,]
  spec_punt_df$punt_id <- i
  spec_punt_df <- spec_punt_df[order(spec_punt_df$y,spec_punt_df$x),]
  spec_punt_punting_df <- spec_punt_df[spec_punt_df$possessionTeam==spec_punt_df$TeamAbbr,]
  spec_punt_punting_df <- rbind(spec_punt_punting_df[spec_punt_punting_df$position=="P",],spec_punt_punting_df[spec_punt_punting_df$position!="P",])
  spec_punt_punting_df <- spec_punt_punting_df[order(spec_punt_punting_df$y,spec_punt_punting_df$x),]
  spec_punt_receiving_df <- spec_punt_df[spec_punt_df$possessionTeam!=spec_punt_df$TeamAbbr,]
  spec_punt_receiving_df <- rbind(spec_punt_receiving_df[spec_punt_receiving_df$x==max(spec_punt_receiving_df$x),],spec_punt_receiving_df[spec_punt_receiving_df$x!=max(spec_punt_receiving_df$x),])
  spec_punt_receiving_df <- spec_punt_receiving_df[order(spec_punt_receiving_df$y,spec_punt_receiving_df$x),]
  punts_lists <- append(punts_lists, list(spec_punt_df, spec_punt_punting_df, spec_punt_receiving_df))
}

cluster_progression <- matrix(nrow=n, ncol=n)
for (i in 1:n) {
  for (j in 1:n) {
    cluster_progression[i, j] <- j
  }
}
cluster_progression <- t(apply(cluster_progression, 1, paste))

while (n >= 2) {
  dist_mat <- matrix(nrow=n, ncol=n)
  for (i in 1:n) {
    for (j in 1:n) {
      total_cluster_distance <- (sqrt(((punts_lists[[(3*i)-1]]$x-punts_lists[[(3*j)-1]]$x)^2)+((punts_lists[[(3*i)-1]]$y-punts_lists[[(3*j)-1]]$y)^2)))+
        (sqrt(((punts_lists[[(3*i)-0]]$x-punts_lists[[(3*j)-0]]$x)^2)+((punts_lists[[(3*i)-0]]$y-punts_lists[[(3*j)-0]]$y)^2)))
      summed_cluster_distance <- (unname(tapply(total_cluster_distance, (seq_along(total_cluster_distance)-1) %/% 11, sum)))
      dist_mat[i, j] <- summed_cluster_distance
    }
  }
  dist_mat[lower.tri(dist_mat, diag=TRUE)] <- NA
  print(dist_mat)
  
  ## this logic is horrible, idk why elegant stuff isn't working
  minindex <- which.min(dist_mat)
  mincol <- (minindex %/% n) + 1
  minrow <- minindex %% n
  
  counter <- 2
  
  if (minrow < mincol) {
    
    cluster_progression[counter, minrow] <- paste(cluster_progression[counter, mincol], cluster_progression[counter, minrow], sep=", ")
    cluster_progression[counter, mincol] <- NA
    
    punts_lists[[3*minrow]]$x <- (1/2)*(punts_lists[[3*minrow]]$x + punts_lists[[3*mincol]]$x)
    punts_lists[[3*minrow]]$y <- (1/2)*(punts_lists[[3*minrow]]$y + punts_lists[[3*mincol]]$y)
    punts_lists[[3*minrow-1]]$x <- (1/2)*(punts_lists[[3*minrow]]$x + punts_lists[[3*mincol-1]]$x)
    punts_lists[[3*minrow-1]]$y <- (1/2)*(punts_lists[[3*minrow]]$y + punts_lists[[3*mincol-1]]$y)
    punts_lists[[3*minrow-2]]$x <- (1/2)*(punts_lists[[3*minrow]]$x + punts_lists[[3*mincol-2]]$x)
    punts_lists[[3*minrow-2]]$y <- (1/2)*(punts_lists[[3*minrow]]$y + punts_lists[[3*mincol-2]]$y)
    
    punts_lists <- punts_lists[-(3*mincol)]
    punts_lists <- punts_lists[-(3*mincol-1)]
    punts_lists <- punts_lists[-(3*mincol-2)]

  } else if (mincol > minrow) {
    
    cluster_progression[counter, mincol] <- paste(cluster_progression[counter, mincol], cluster_progression[counter, minrow], sep=", ")
    cluster_progression[counter, minrow] <- NA
    
    punts_lists[[3*mincol]]$x <- (1/2)*(punts_lists[[3*mincol]]$x + punts_lists[[3*minrow]]$x)
    punts_lists[[3*mincol]]$y <- (1/2)*(punts_lists[[3*mincol]]$y + punts_lists[[3*minrow]]$y)
    punts_lists[[3*mincol-1]]$x <- (1/2)*(punts_lists[[3*mincol-1]]$x + punts_lists[[3*minrow-1]]$x)
    punts_lists[[3*mincol-1]]$y <- (1/2)*(punts_lists[[3*mincol-1]]$y + punts_lists[[3*minrow-1]]$y)
    punts_lists[[3*mincol-2]]$x <- (1/2)*(punts_lists[[3*mincol-2]]$x + punts_lists[[3*minrow-2]]$x)
    punts_lists[[3*mincol-2]]$y <- (1/2)*(punts_lists[[3*mincol-2]]$y + punts_lists[[3*minrow-2]]$y)
    
    punts_lists <- punts_lists[-(3*minrow)]
    punts_lists <- punts_lists[-(3*minrow-1)]
    punts_lists <- punts_lists[-(3*minrow-2)]
    
  }
  n <- n-1
  counter <- counter + 1
}

####

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