library(ggplot2)
library(dplyr)
library(gtools)
library(data.table)
library(cluster)
library(tidyverse)

### run clustering_scratchwork.R before this

## clusters sample into 1 group step-by-step
## currently prints distance matrices as we go

## function for cleanliness later on
punt_dist <- function(id1, id2, plist) {
  total_cluster_distance <- (sqrt(((plist[[(3*id1)-1]]$x-plist[[(3*id2)-1]]$x)^2)+((plist[[(3*id1)-1]]$y-plist[[(3*id2)-1]]$y)^2)))+
    (sqrt(((plist[[(3*id1)-0]]$x-plist[[(3*id2)-0]]$x)^2)+((plist[[(3*id1)-0]]$y-plist[[(3*id2)-0]]$y)^2)))
  summed_cluster_distance <- (unname(tapply(total_cluster_distance, (seq_along(total_cluster_distance)-1) %/% 11, sum)))
  return(summed_cluster_distance)
}

## sets sample size
n <- (nrow(frame_punt_events_df)/22)-1

## makes list containing 3 punt dataframes for every 1 in sample
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

index_vec <- c(rep(0,n)) #needs to be outside loop 
info_df <- as.data.frame(matrix(NA, ncol = n, nrow = n))
info_df[1,] <- c(1:n)
new_n <- n

while (n>=2) {
  
  ## calculates distance matrix for punts left in the list 
  dist_mat <- matrix(nrow=n, ncol=n)
  for (i in 1:n) {
    for (j in 1:n) {
      dist_mat[i, j] <- punt_dist(i, j, punts_lists)
    }
  }
  ## removes zeroes
  dist_mat[lower.tri(dist_mat, diag=TRUE)] <- NA
  # print(dist_mat)
  
  ##logic to find minimum row and column
  # minindex <- which.min(dist_mat)
  # mincol <- (minindex %/% n) + 1
  # minrow <- minindex %% n
  
  dist_mat[is.na(dist_mat)] <- Inf
  loc_mat <- as.data.frame(which(dist_mat==min(dist_mat), arr.ind=TRUE))
  minrow <- as.numeric(loc_mat$row[1])
  mincol <- as.numeric(loc_mat$col[1])
  # print(minrow)
  # print(mincol)
  
  preserved_cluster <- min(c(mincol, minrow))
  unpreserved_cluster <- max(c(mincol, minrow))
  
  ## takes mean of two closest punts and sets it as new value for the first punt
  ## e.g. if punt 6 and 12 are closest, takes the means of x and y and updates punt 6 with new values
  punts_lists[[3*preserved_cluster]]$x <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster])))+1))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster]))))*punts_lists[[3*preserved_cluster]]$x + punts_lists[[3*unpreserved_cluster]]$x)
  
  punts_lists[[3*preserved_cluster]]$y <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster])))+1))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster]))))*punts_lists[[3*preserved_cluster]]$y + punts_lists[[3*unpreserved_cluster]]$y)
  
  punts_lists[[3*preserved_cluster-1]]$x <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster])))+1))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster]))))*punts_lists[[3*preserved_cluster-1]]$x + punts_lists[[3*unpreserved_cluster-1]]$x)
  
  punts_lists[[3*preserved_cluster-1]]$y <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster])))+1))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster]))))*punts_lists[[3*preserved_cluster-1]]$y + punts_lists[[3*unpreserved_cluster-1]]$y)
  
  punts_lists[[3*preserved_cluster-2]]$x <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster])))+1))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster]))))*punts_lists[[3*preserved_cluster-2]]$x + punts_lists[[3*unpreserved_cluster-2]]$x)
  
  punts_lists[[3*preserved_cluster-2]]$y <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster])))+1))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster]))))*punts_lists[[3*preserved_cluster-2]]$y + punts_lists[[3*unpreserved_cluster-2]]$y)
  
  ## removes latter punt (12 in example above)
  punts_lists <- punts_lists[-(3*unpreserved_cluster)]
  punts_lists <- punts_lists[-(3*unpreserved_cluster-1)]
  punts_lists <- punts_lists[-(3*unpreserved_cluster-2)]
  
  print(dist_mat)
  print(loc_mat)

  print(index_vec)
  preserved_cluster_id <- preserved_cluster+index_vec[preserved_cluster]
  unpreserved_cluster_id <- unpreserved_cluster+index_vec[unpreserved_cluster]
  index_vec[unpreserved_cluster:new_n] <- index_vec[unpreserved_cluster:new_n]+1
  index_vec <- index_vec[-unpreserved_cluster]
  
  print(preserved_cluster_id)
  print(unpreserved_cluster_id)
  
  ids_in_cluster <- c(as.vector(na.omit(info_df[,preserved_cluster_id])),as.vector(na.omit(info_df[,unpreserved_cluster_id])))
  info_df[,preserved_cluster_id] <- c(ids_in_cluster,rep(NA,new_n-length(ids_in_cluster)))
  info_df[,unpreserved_cluster_id] <- NA
  print(info_df)
  
  n <- n-1
  
}
  

  

  
  
  
  
