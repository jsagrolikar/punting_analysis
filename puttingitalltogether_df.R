## the big kahuna

library(ggplot2)
library(dplyr)
library(gtools)
library(data.table)
library(cluster)
library(tidyverse)

wd <- "C:/Users/Jay Sagrolikar/punting_analysis/data"
setwd(wd)
# tracking_sample <- read.csv("tracking2020.csv")
tracking_sample <- read.csv("2020_reduced.csv")
tracking_sample$ï..time <- NULL
games_df <- read.csv("games.csv")
plays_df <- read.csv("plays.csv")
players_df <- read.csv("players.csv")

## function for cleanliness later on
punt_dist <- function(id1, id2, plist) {
  total_cluster_distance <- (sqrt(((plist[[(3*id1)-1]]$x-plist[[(3*id2)-1]]$x)^2)+((plist[[(3*id1)-1]]$y-plist[[(3*id2)-1]]$y)^2)))+
    (sqrt(((plist[[(3*id1)-0]]$x-plist[[(3*id2)-0]]$x)^2)+((plist[[(3*id1)-0]]$y-plist[[(3*id2)-0]]$y)^2)))
  summed_cluster_distance <- (unname(tapply(total_cluster_distance, (seq_along(total_cluster_distance)-1) %/% 11, sum)))
  return(summed_cluster_distance)
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

## modifying left direction coordinates, binding to right
left_direction_df <- tracking_sample[tracking_sample$playDirection=="left",]
left_direction_df$y <- 53.3-left_direction_df$y
left_direction_df$x <- 120-left_direction_df$x
tracking_sample <- tracking_sample[tracking_sample$playDirection!="left",]
tracking_sample <- rbind(tracking_sample, left_direction_df)

# blocked_punts <- tracking_sample[tracking_sample$event=="punt_blocked",]
# blocked_punt_summary <- unique(data.frame(gameId=blocked_punts$gameId, playId=blocked_punts$playId, frameId=blocked_punts$frameId))
# blocked_punts_df <- tracking_sample[tracking_sample$gameId %in% blocked_punt_summary$gameId & tracking_sample$playId %in% blocked_punt_summary$playId,]
# write.csv(exp_df, "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022\\2020_reduced.csv")
# blocked_punts_df$time <- NULL
# exp_df <- rbind(tracking_sample, blocked_punts_df)

## filtering for punts
poss_team_df <- data.frame(possessionTeam=plays_df$possessionTeam, gameId=plays_df$gameId, playId=plays_df$playId)
punt_instance_df <- tracking_sample[tracking_sample$event=="punt",]
punt_info_index <- unique(data.frame(gameId=punt_instance_df$gameId, playId=punt_instance_df$playId, punt_event_frame=punt_instance_df$frameId))
punt_events_df <- merge(tracking_sample, punt_info_index, by=c("gameId", "playId"))

## gets football location data and makes a df with LOS information
ball_snap_punt_df <- punt_events_df[punt_events_df$event=="ball_snap",]
ball_snap_punt_info_index <- unique(data.frame(gameId=ball_snap_punt_df$gameId, playId=ball_snap_punt_df$playId, ball_snap_event_frame=ball_snap_punt_df$frameId))
punt_info_index <- merge(punt_info_index, ball_snap_punt_info_index, by=c("gameId","playId"))
punt_events_df <- merge(tracking_sample, punt_info_index, by=c("gameId", "playId"))
punt_events_ball_at_snap_df <- punt_events_df[punt_events_df$displayName=="football" & punt_events_df$event=="ball_snap", ]
los_info_df <- data.frame(gameId=punt_events_ball_at_snap_df$gameId,playId=punt_events_ball_at_snap_df$playId,los_x=punt_events_ball_at_snap_df$x, los_y=punt_events_ball_at_snap_df$y)
punt_events_df$frames_after_snap <- punt_events_df$frameId-punt_events_df$ball_snap_event_frame

## no football rows
punt_events_df <- punt_events_df[!punt_events_df$displayName=="football", ]

## combines original punt data with LOS data
# punt_events_df$rounded_y <- round(punt_events_df$y)
punt_events_df <- merge(punt_events_df, games_df, by=c("gameId"))
punt_events_df <- merge(punt_events_df, poss_team_df, by=c("gameId", "playId"))
punt_events_df$TeamAbbr <- NA
punt_events_df$TeamAbbr[punt_events_df$team=="home"] <- punt_events_df$homeTeamAbbr[punt_events_df$team=="home"]
punt_events_df$TeamAbbr[punt_events_df$team=="away"] <- punt_events_df$visitorTeamAbbr[punt_events_df$team=="away"]
punt_events_df <- merge(punt_events_df, los_info_df, by=c("gameId","playId"))
punt_events_df$x <- punt_events_df$x-punt_events_df$los_x
punt_events_df$y <- punt_events_df$y-punt_events_df$los_y 
## unsure if we should normalize y to LOS; likely insignificant but should discuss

## filtering for frames within snap and punt
filt_punt_events_df <- punt_events_df[punt_events_df$frames_after_snap>=0 & punt_events_df$frames_after_snap<=(punt_events_df$punt_event_frame-punt_events_df$ball_snap_event_frame),]
max(filt_punt_events_df$frames_after_snap)

aggregate(frame_id~punt_id, filt_punt_events_df)
sort((filt_punt_events_df$frames_after_snap))
for (frame_id in c(0:25)) {
  frame_punt_events_df <- filt_punt_events_df[filt_punt_events_df$frames_after_snap==frame_id,]
  frame_punt_events_df<- frame_punt_events_df[order(frame_punt_events_df$y, frame_punt_events_df$x),]
  # punt2_frame0_punting_df <- punt2_frame0_df[punt2_frame0_df$possessionTeam==punt2_frame0_df$TeamAbbr,]
  punt_index_df <- (unique(data.frame(gameId=frame_punt_events_df$gameId, playId=frame_punt_events_df$playId)))
  punt_index_df$punt_id <- 1:nrow(punt_index_df)
  row.names(punt_index_df) <- NULL
  
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
  
  while (n>=2) {
    
    dist_mat <- make_dist_mat(n, punts_lists)
    
    loc_mat <- as.data.frame(which(dist_mat==min(dist_mat), arr.ind=TRUE))
    minrow <- as.numeric(loc_mat$row[1])
    mincol <- as.numeric(loc_mat$col[1])
    
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
    
    preserved_cluster_id <- preserved_cluster+index_vec[preserved_cluster]
    unpreserved_cluster_id <- unpreserved_cluster+index_vec[unpreserved_cluster]
    index_vec[unpreserved_cluster:new_n] <- index_vec[unpreserved_cluster:new_n]+1
    index_vec <- index_vec[-unpreserved_cluster]
    
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
  result <- information_list[[mincluster]]
  print(frame_id)
  print(result)
  
}