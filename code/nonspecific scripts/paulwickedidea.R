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
  # print(total_cluster_distance)
  # print(sum(total_cluster_distance))
  # summed_cluster_distance <- (unname(tapply(total_cluster_distance, (seq_along(total_cluster_distance)-1) %/% 11, sum)))
  return(sum(total_cluster_distance))
}

make_dist_mat <- function(p, plist) {
  ## calculates distance matrix for punts left in the list 
  dist_mat <- matrix(nrow=p, ncol=p)
  for (i in 1:p) {
    for (j in 1:p) {
      dist_mat[i, j] <- punt_dist(i, j, plist)
    }
  }
  ## removes zeroes
  dist_mat[lower.tri(dist_mat, diag=TRUE)] <- Inf
  return(dist_mat)
}

make_dist_mat2 <- function(p, idlist, plist) {
  ## calculates distance matrix for punts left in the list 
  dist_mat <- matrix(nrow=p, ncol=p)
  for (i in 1:(p-1)) {
    id1 <- idlist[[i]]
    for (j in 1:(p-1)) {
      id2 <- idlist[[j+1]]
      dist_mat[i, j+1] <- punt_dist(id1, id2, plist)
    }
  }
  ## removes zeroes
  dist_mat[lower.tri(dist_mat, diag=TRUE)] <- Inf
  return(dist_mat)
}

na.omit.list <- function(y) { 
  return(y[!sapply(y, function(x) all(is.na(x)))]) 
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
all_punts <- punts_lists

index_vec <- c(rep(0,n)) #needs to be outside loop 
info_df <- as.data.frame(matrix(NA, ncol = n, nrow = n))
info_df[1,] <- c(1:n)
new_n <- n

iteration_scores <- as.data.frame(matrix(0, ncol = 2, nrow = n))
colnames(iteration_scores) <- c("number of clusters", "average distance within clusters")
iteration_scores[new_n, 1] <- new_n
information_list <- list()
preserved_cluster <- NA

while (n>=14) {
  
  if (is.na(preserved_cluster)==FALSE) {
    
    dist_mat[unpreserved_cluster,]*punts_in_unpreserved_cluster
    dist_mat[,unpreserved_cluster]*punts_in_unpreserved_cluster
    
    dist_mat[preserved_cluster,-unpreserved_cluster]*punts_in_preserved_cluster
    dist_mat[,preserved_cluster]*punts_in_preserved_cluster
    
    dist_mat <- dist_mat[-unpreserved_cluster,-unpreserved_cluster]
    dist_vec1 <- c(rep(Inf,(n)))
    dist_vec2 <- c(rep(Inf,(n)))
    for (i in 1:preserved_cluster) {
      print(i)
      dist_vec1[i] <- punt_dist(preserved_cluster, i, punts_lists)
    }
    for (i in preserved_cluster:n) {
      print(i)
      dist_vec2[i] <- punt_dist(preserved_cluster, i, punts_lists)
    }
    dist_mat[,preserved_cluster] <- dist_vec1
    dist_mat[preserved_cluster,] <- dist_vec2
    dist_mat[lower.tri(dist_mat, diag=TRUE)] <- Inf
    
  } else {
    dist_mat <- make_dist_mat(n, punts_lists)
  }
  
  loc_mat <- as.data.frame(which(dist_mat==min(dist_mat), arr.ind=TRUE))
  minrow <- as.numeric(loc_mat$row[1])
  mincol <- as.numeric(loc_mat$col[1])
  
  preserved_cluster <- min(c(mincol, minrow))
  unpreserved_cluster <- max(c(mincol, minrow))
  preserved_cluster_id <- preserved_cluster+index_vec[preserved_cluster]
  unpreserved_cluster_id <- unpreserved_cluster+index_vec[unpreserved_cluster]
  
  ## takes mean of two closest punts and sets it as new value for the first punt
  ## e.g. if punt 6 and 12 are closest, takes the means of x and y and updates punt 6 with new values
  punts_lists[[3*preserved_cluster]]$x <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster]]$x + 
       (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster]]$x)
  
  punts_lists[[3*preserved_cluster]]$y <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster]]$y + 
       (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster]]$y)
  
  punts_lists[[3*preserved_cluster-1]]$x <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster-1]]$x + 
       (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster-1]]$x)
  
  punts_lists[[3*preserved_cluster-1]]$y <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster-1]]$y + 
       (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster-1]]$y)
  
  punts_lists[[3*preserved_cluster-2]]$x <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster-2]]$x + 
       (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster-2]]$x)
  
  punts_lists[[3*preserved_cluster-2]]$y <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
    ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster-2]]$y + 
       (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster-2]]$y)
  
  ## removes latter punt (12 in example above)
  punts_lists <- punts_lists[-(3*unpreserved_cluster)]
  punts_lists <- punts_lists[-(3*unpreserved_cluster-1)]
  punts_lists <- punts_lists[-(3*unpreserved_cluster-2)]
  
  index_vec[unpreserved_cluster:new_n] <- index_vec[unpreserved_cluster:new_n]+1
  index_vec <- index_vec[-unpreserved_cluster]
  
  punts_in_unpreserved_cluster <- (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))
  punts_in_preserved_cluster <- (length(as.vector(na.omit(info_df[,preserved_cluster_id]))))
  
  ids_in_cluster <- c(as.vector(na.omit(info_df[,preserved_cluster_id])),as.vector(na.omit(info_df[,unpreserved_cluster_id])))
  info_df[,preserved_cluster_id] <- c(ids_in_cluster,rep(NA,new_n-length(ids_in_cluster)))
  info_df[,unpreserved_cluster_id] <- NA
  
  if (n == new_n) {
    iteration_scores[1, 1] <- 1
    iteration_scores[1, 2] <- mean(dist_mat[is.finite(dist_mat)])
  }
  
  cluster_df <- info_df[,colSums(is.na(info_df))<nrow(info_df)]
  information_list[[n-1]] <- as.data.frame(cluster_df)
  # information_list <- append(information_list, as.data.frame(cluster_df))
  total_cluster_distances <- 0
  if (!is.null(ncol(cluster_df))) {
    for (i in 1:ncol(cluster_df)) {
      punt_ids <- list()
      distancescore <- 0
      for (j in 1:nrow(cluster_df)) {
        punt_ids <- append(punt_ids, cluster_df[j, i])
      }
      punt_ids <- na.omit.list(punt_ids)
      if (length(punt_ids) <= 1) {
        cluster_dist <- 0
      } else {
        cluster_mat <- make_dist_mat2(length(punt_ids), punt_ids, all_punts)
        cluster_dist <- mean(cluster_mat[is.finite(cluster_mat)])
      }
      distancescore <- distancescore + cluster_dist
      total_cluster_distances <- total_cluster_distances + distancescore
      
    }
    
    total_cluster_distances <- total_cluster_distances/ncol(cluster_df)
    
    iteration_scores[n-1, 1] <- ncol(cluster_df)
    iteration_scores[n-1, 2] <- total_cluster_distances
    iteration_scores$total_distance <- iteration_scores$`number of clusters`*iteration_scores$`average distance within clusters`
    
  }
  
  n <- n-1
  
}

iteration_scores$iteration_scores <- (iteration_scores$`number of clusters`/max(iteration_scores$`number of clusters`))+
  (iteration_scores$`average distance within clusters`/max(iteration_scores$`average distance within clusters`))
mincluster <- iteration_scores[iteration_scores$iteration_scores==min(iteration_scores$iteration_scores),]$`number of clusters`
finalclusters <- information_list[[mincluster]]
result <- finalclusters

for (i in 1:ncol(result)) {
  for (j in 1:nrow(result)) {
    if (is.na(result[j, i])) {
      result[j, i] <- "0"
    } else {
      result[j, i] <- paste(as.character(all_punts[[3*as.numeric(result[j,i])]]$gameId[1]), "/", as.character(all_punts[[3*as.numeric(result[j,i])]]$playId[1]), sep="")
    }
  }
}

