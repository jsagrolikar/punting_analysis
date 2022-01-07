## the big kahuna
## run gc() before this
library(MASS)
library(viridis)
library(ggplot2)
library(dplyr)
library(gtools)
library(data.table)
library(cluster)
library(tidyverse)

setwd("C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022")
# tracking_sample <- read.csv("all_punts.csv")
tracking_sample <- read.csv("1000sample.csv")
games_df <- read.csv("games.csv")
plays_df <- read.csv("plays.csv")
players_df <- read.csv("players.csv")


## modifying left direction coordinates, binding to right
left_direction_df <- tracking_sample[tracking_sample$playDirection=="left",]
left_direction_df$y <- 53.3-left_direction_df$y
left_direction_df$x <- 120-left_direction_df$x
tracking_sample <- tracking_sample[tracking_sample$playDirection!="left",]
tracking_sample <- rbind(tracking_sample, left_direction_df)

## filtering for punts
poss_team_df <- data.frame(possessionTeam=plays_df$possessionTeam, gameId=plays_df$gameId, playId=plays_df$playId)
punt_instance_df <- tracking_sample[tracking_sample$event=="punt",]
punter_df <- (punt_instance_df[punt_instance_df$position=="P",])
punter_info_df <- data.frame(playId=punter_df$playId, gameId=punter_df$gameId, punting_team=punter_df$team)

punt_instance_df <- merge(punter_info_df, punt_instance_df, by=c("gameId","playId"))
punt_info_index <- unique(data.frame(gameId=punt_instance_df$gameId, playId=punt_instance_df$playId, punt_event_frame=punt_instance_df$frameId, punting_team=punt_instance_df$punting_team))
punt_events_df <- merge(tracking_sample, punt_info_index, by=c("gameId", "playId"))
# punt_events_df <- merge(punt_events_df, punter_loc_info_df, by=c("gameId", "playId", "punting_team"))

## gets football location data and makes a df with LOS information
ball_snap_punt_df <- punt_events_df[punt_events_df$event=="ball_snap",]
ball_snap_punt_info_index <- unique(data.frame(gameId=ball_snap_punt_df$gameId, playId=ball_snap_punt_df$playId, ball_snap_event_frame=ball_snap_punt_df$frameId))
punt_info_index <- merge(punt_info_index, ball_snap_punt_info_index, by=c("gameId","playId"))
punt_events_df <- merge(tracking_sample, punt_info_index, by=c("gameId", "playId"))


# punt_events_df <- merge(punt_events_df, punt_info_index2, by=c("gameId", "playId"))
punt_events_ball_at_snap_df <- punt_events_df[punt_events_df$displayName=="football" & punt_events_df$event=="ball_snap", ]
los_info_df <- data.frame(gameId=punt_events_ball_at_snap_df$gameId,playId=punt_events_ball_at_snap_df$playId,los_x=punt_events_ball_at_snap_df$x, los_y=punt_events_ball_at_snap_df$y)
punt_events_df$frames_after_snap <- punt_events_df$frameId-punt_events_df$ball_snap_event_frame

## no football rows
football_punt_events_df <- punt_events_df[punt_events_df$displayName=="football", ]
punt_events_df <- punt_events_df[!punt_events_df$displayName=="football", ]


## combines original punt data with LOS data
punt_events_df <- merge(punt_events_df, games_df, by=c("gameId"))
punt_events_df <- merge(punt_events_df, poss_team_df, by=c("gameId", "playId"))
punt_events_df$TeamAbbr <- NA
punt_events_df$TeamAbbr[punt_events_df$team=="home"] <- punt_events_df$homeTeamAbbr[punt_events_df$team=="home"]
punt_events_df$TeamAbbr[punt_events_df$team=="away"] <- punt_events_df$visitorTeamAbbr[punt_events_df$team=="away"]
punt_events_df <- merge(punt_events_df, los_info_df, by=c("gameId","playId"))
punt_events_df$x <- punt_events_df$x-punt_events_df$los_x
punt_events_df$y <- punt_events_df$y-punt_events_df$los_y 
punter_df2 <- (punt_events_df[punt_events_df$position=="P",])
punter_loc_info_df <- data.frame(playId=punter_df2$playId, gameId=punter_df2$gameId, frameId=punter_df2$frameId, punting_team=punter_df2$team, punter_x=punter_df2$x, punter_y=punter_df2$y)
punt_events_df <- merge(punt_events_df, punter_loc_info_df, by=c("gameId", "playId","frameId", "punting_team"))

punt_events_df$adj_x <- punt_events_df$x-punt_events_df$punter_x
punt_events_df$adj_y <- punt_events_df$y-punt_events_df$punter_y
punt_events_df$rad_angle <- atan(punt_events_df$adj_y/punt_events_df$adj_x)
punt_events_df$deg_angle <-  punt_events_df$rad_angle*(180/pi)
punt_events_df$dist_to_punter <- sqrt((punt_events_df$adj_x^2)+(punt_events_df$adj_y^2))

filt_punt_events_df <- punt_events_df[punt_events_df$frames_after_snap>=0 & punt_events_df$frames_after_snap<=(punt_events_df$punt_event_frame-punt_events_df$ball_snap_event_frame),]

## function for cleanliness later on
punt_dist <- function(id1, id2, plist) {
  bin_diff <- (abs(plist[[(3*id1)-1]]$density-plist[[(3*id2)-1]]$density)+abs(plist[[(3*id1)-0]]$density-plist[[(3*id2)-0]]$density))
  return(sum(bin_diff))
}

punt_dist2 <- function(id1, id2, plist, max) {
  bin_diff <- (abs(plist[[(3*id1)-1]]$density-plist[[(3*id2)-1]]$density)+abs(plist[[(3*id1)-0]]$density-plist[[(3*id2)-0]]$density))/max
  return(sum(bin_diff))
}

make_dist_mat <- function(p, plist) {
  ## calculates distance matrix for punts left in the list 
  dist_mat <- matrix(nrow=p, ncol=p)
  # dist_mat <- outer(1:p, 1:p, Vectorize(function(x, y) punt_dist(x, y, plist)))
  for (i in 1:(p-1)) {
    dist_mat[i, ] <- c(1:p)
    vec <- sapply(dist_mat[i, (i+1):p], function(x) punt_dist(i, x, plist))
    dist_mat[i, ] <- c(rep(Inf, i), vec)
  }
  ## removes zeroes
  dist_mat[lower.tri(dist_mat, diag=TRUE)] <- Inf
  return(dist_mat)
}


clusterdistance <- function(p, idlist, initmat) {
  ## calculates distance matrix for punts left in the list 
  dist_mat <- matrix(nrow=p, ncol=p)
  im <- initmat
  cluster_mat <- im[idlist, idlist]
  cluster_mat[cluster_mat==Inf] <- 0
  cluster_distance <- mean(cluster_mat)
  # dist_mat <- outer(1:p, 1:p, Vectorize(function(x, y) if(x < y) { im[idlist[[x]], idlist[[y]]] } else { im[idlist[[y]], idlist[[x]]]}))
  # for (i in 1:(p-1)) {
  #   dist_mat[i, ] <- c(1:p)
  #   vec <- sapply(dist_mat[i, (i+1):p], function(x) if(i < x) { im[idlist[[i]], idlist[[x]]] } else { im[idlist[[x]], idlist[[i]]]})
  #   dist_mat[i, ] <- c(rep(Inf, i), vec)
  # }
  ## removes zeroes
  
  return(cluster_distance)
}

na.omit.list <- function(y) { 
  return(y[!sapply(y, function(x) all(is.na(x)))]) 
}


result_list <- list()
for (frame_id in c(1,5,10,15,20)) {
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

    other_punting_players_df <- spec_punt_df[spec_punt_df$team==spec_punt_df$punting_team,]
    other_punting_players_df <- other_punting_players_df[other_punting_players_df$position!="P",]
    
    other_receiving_players_df <- spec_punt_df[spec_punt_df$team!=spec_punt_df$punting_team,]
    
    punter_kd <- other_punting_players_df %>%
      with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                       lims = c(
                         scales::expand_range(c(-180,180), .0),
                         scales::expand_range(c(0,75), .0))))
    punter_kd_df <- punter_kd %>%
      .[c("x", "y")] %>%
      cross_df() %>%
      rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
      mutate(density = as.vector(punter_kd$z))
    
    punter_rec_kd <- other_receiving_players_df %>%
      with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                       lims = c(
                         scales::expand_range(c(-180,180), .0),
                         scales::expand_range(c(0,75), .0))))
    punter_rec_kd_df <- punter_rec_kd %>%
      .[c("x", "y")] %>%
      cross_df() %>%
      rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
      mutate(density = as.vector(punter_rec_kd$z))
    
    pt_bin_diff <- abs(mean_punt_pt_dens-(punter_kd_df$density/sum(punter_kd_df$density)))*pt_list[[frame_id]]
    rt_bin_diff <- abs(mean_punt_rt_dens-(punter_rec_kd_df$density/sum(punter_rec_kd_df$density)))*rt_list[[frame_id]]

    punts_lists <- append(punts_lists, list(spec_punt_df, pt_bin_diff, rt_bin_diff))
  }
  print("list complete")
  all_punts <- punts_lists
  # punts_lists <- all_punts
  
  index_vec <- c(rep(0,n)) #needs to be outside loop 
  info_df <- as.data.frame(matrix(NA, ncol = n, nrow = n))
  info_df[1,] <- c(1:n)
  new_n <- n
  
  iteration_scores <- as.data.frame(matrix(0, ncol = 2, nrow = n))
  colnames(iteration_scores) <- c("number of clusters", "average distance within clusters")
  iteration_scores[new_n, 1] <- new_n
  information_list <- list()
  preserved_cluster <- NA
  
  # n <- 432
  while (n>=2) {
    if (is.na(preserved_cluster)==FALSE) {
      

      dist_mat <- dist_mat[-unpreserved_cluster,-unpreserved_cluster]
      dist_vec1 <- c(1:(n))
      dist_vec1 <- sapply(dist_vec1, function(x) (punt_dist2(preserved_cluster, x, punts_lists, init_mat_max)^(1-((length(ids_in_cluster))/(new_n/2)))))
      dist_vec2 <- c(rep(Inf, (preserved_cluster-1)), dist_vec1[preserved_cluster:(n)])
      dist_mat[,preserved_cluster] <- dist_vec1
      dist_mat[preserved_cluster,] <- dist_vec2
      
      dist_mat[lower.tri(dist_mat, diag=TRUE)] <- Inf
      dist_mat <- dist_mat/(max(dist_mat[dist_mat!=Inf]))
      
    } else {
      init_mat <- make_dist_mat(n, punts_lists)
      init_mat_max <- max(init_mat[init_mat!=Inf])
      dist_mat <- init_mat
      dist_mat <- dist_mat/init_mat_max
      print("init matrix complete")
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
    
    punts_lists[[3*preserved_cluster]]$density <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
      ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster]]$density + 
         (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster]]$density)
    
    punts_lists[[3*preserved_cluster-1]]$density <- (1/(length(as.vector(na.omit(info_df[,preserved_cluster_id])))+length(as.vector(na.omit(info_df[,unpreserved_cluster_id])))))*
      ((length(as.vector(na.omit(info_df[,preserved_cluster_id]))))*punts_lists[[3*preserved_cluster-1]]$density + 
         (length(as.vector(na.omit(info_df[,unpreserved_cluster_id]))))*punts_lists[[3*unpreserved_cluster-1]]$density)
    
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
    
    ids_in_cluster <- c(as.vector(na.omit(info_df[,preserved_cluster_id])),as.vector(na.omit(info_df[,unpreserved_cluster_id])))
    info_df[,preserved_cluster_id] <- c(ids_in_cluster,rep(NA,new_n-length(ids_in_cluster)))
    info_df[,unpreserved_cluster_id] <- NA
    
    if (n == new_n) {
      iteration_scores[1, 1] <- 1
      iteration_scores[1, 2] <- mean(dist_mat[is.finite(dist_mat)])
    }
    
    cluster_df <- as.data.frame(info_df[,colSums(is.na(info_df))<nrow(info_df)])
    information_list[[n-1]] <- cluster_df
    nonzero_clusters <- as.data.frame(cluster_df[,colSums(is.na(cluster_df))<(nrow(cluster_df)-1)])
    total_cluster_distances <- 0
    if (!is.null(ncol(nonzero_clusters))) {
      
      cluster_dist <- apply(nonzero_clusters, 2, function(x) clusterdistance(length(na.omit(x)), na.omit(x), init_mat))
      
      # cluster_dist <- apply(nonzero_clusters, 2,
      #                       function(x)
      #                         mean(make_dist_mat2(length(na.omit(x)), x[!is.na(x)], init_mat)[is.finite(make_dist_mat2(length(na.omit(x)), x[!is.na(x)], init_mat))]))
      
      # cluster_dist <- c(1:ncol(nonzero_clusters))
      # for (i in 1:ncol(nonzero_clusters)) {
      #   cluster_dist[i] <- mean(make_dist_mat2(length(na.omit(nonzero_clusters[,i])), nonzero_clusters[,i][!is.na(nonzero_clusters[,i])], init_mat)[is.finite(make_dist_mat2(length(na.omit(nonzero_clusters[,i])), nonzero_clusters[,i][!is.na(nonzero_clusters[,i])], init_mat))])
      # }
      
      total_cluster_distances <- sum(cluster_dist)
      
    } else {
      total_cluster_distances <- punt_dist(nonzero_clusters[1], nonzero_clusters[2], all_punts)
    }
    
    total_cluster_distances <- total_cluster_distances/ncol(cluster_df)
    
    iteration_scores[n-1, 1] <- ncol(cluster_df)
    iteration_scores[n-1, 2] <- total_cluster_distances
    iteration_scores$total_distance <- iteration_scores$`number of clusters`*iteration_scores$`average distance within clusters`
    
    n <- n-1
    
  }
  
  k <- 0.6
  iteration_scores$iteration_scores <- k*(iteration_scores$`number of clusters`/max(iteration_scores$`number of clusters`))+
    (1-k)*(iteration_scores$`average distance within clusters`/max(iteration_scores$`average distance within clusters`))
  mincluster <- iteration_scores[iteration_scores$iteration_scores==min(iteration_scores$iteration_scores),]$`number of clusters`
  finalclusters <- information_list[[mincluster]]
  result <- finalclusters
  result_list[[frame_id]] <- result
  
  clusters_to_save <- (iteration_scores[order(iteration_scores$iteration_scores),]$`number of clusters`)[1:100]
  sdir <- "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022/saved_clusters/"
  for(i in na.omit(clusters_to_save)) {
    fname <- paste("frame", as.character(frame_id), "_", as.character(which(clusters_to_save==i)[1]), "scorer.csv", sep="")
    write.csv(information_list[[i]], paste(sdir, fname, sep=""), row.names = FALSE)
  }
  
  if (is.null(result)) {
    break
  } else {
    for (i in 1:ncol(result)) {
      for (j in 1:nrow(result)) {
        if (is.na(result[j, i])) {
          result[j, i] <- "0"
        } else {
          result[j, i] <- paste(as.character(all_punts[[3*as.numeric(result[j,i])-2]]$gameId[1]), "/", as.character(all_punts[[3*as.numeric(result[j,i])-2]]$playId[1]), sep="")
        }
      }
    }
    
    dir <- "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022/Output2"
    filename <- paste(as.character(frame_id), "iteration.csv", sep="")
    fname2 <- paste(as.character(frame_id), "scores.csv")
    print("Frame Iteration")
    print(frame_id)
    write.csv(result, paste(dir, filename, sep=""), row.names = FALSE)
    write.csv(iteration_scores, paste(dir, fname2, sep=""), row.names = FALSE)
  }
}

