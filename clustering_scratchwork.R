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

for (frame_id in c(0:max(filt_punt_events_df$frames_after_snap))[1]) {
  frame_punt_events_df <- filt_punt_events_df[filt_punt_events_df$frames_after_snap==frame_id,]
  frame_punt_events_df<- frame_punt_events_df[order(frame_punt_events_df$y, frame_punt_events_df$x),]
  # punt2_frame0_punting_df <- punt2_frame0_df[punt2_frame0_df$possessionTeam==punt2_frame0_df$TeamAbbr,]
  punt_index_df <- (unique(data.frame(gameId=frame_punt_events_df$gameId, playId=frame_punt_events_df$playId)))
  punt_index_df$punt_id <- 1:nrow(punt_index_df)
  row.names(punt_index_df) <- NULL
  
  n <- (nrow(punt_index_df)-1)
  dist_mat <- matrix(nrow=n, ncol=n)
  formation_list <- list()
  for (i in c(1:n)) {
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
    dist_mat[i,] <- c(rep(NA,(i-1)), (summed_cluster_distance))
    formation_list[[i]] <- formation_comp_df
    # data.frame("kicking_team")
    # print(total_cluster_distance)

  }
}