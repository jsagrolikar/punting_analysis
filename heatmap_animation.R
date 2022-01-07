## the big kahuna
## run gc() before this
library(MASS)
library(viridis)
library(ggplot2)
library(dplyr)
library(gtools)
library(data.table)
library(cluster)
library(gganimate)
library(cowplot)
library(tidyverse)

setwd("C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022")
# tracking_sample <- read.csv("all_punts.csv")
tracking_sample <- read.csv("sampleplayfull.csv")
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

# filt_punt_events_df <- punt_events_df[punt_events_df$frames_after_snap>=0 & punt_events_df$frames_after_snap<=(punt_events_df$punt_event_frame-punt_events_df$ball_snap_event_frame),]
# filt_punt_events_df <- filt_punt_events_df[filt_punt_events_df$frames_after_snap%in%c(1:25),]
filt_punt_events_df <- punt_events_df
unique(filt_punt_events_df$frames_after_snap)
punting_kd_list <- list()
receiving_kd_list <- list()
pplot_list <- list()
rplot_list <- list()
for (frame_id in c(1:25)) {
  spec_punt_df <- filt_punt_events_df[filt_punt_events_df$frames_after_snap==frame_id,]

  other_punting_players_df <- spec_punt_df[spec_punt_df$team==spec_punt_df$punting_team,]
  other_punting_players_df <- other_punting_players_df[other_punting_players_df$position!="P",]
  
  other_receiving_players_df <- spec_punt_df[spec_punt_df$team!=spec_punt_df$punting_team,]
  other_receiving_players_df <- other_receiving_players_df[other_receiving_players_df$position!="P",]
  
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
  punter_kd_df$frames_after_snap <- frame_id
  punting_kd_list[[frame_id]] <- punter_kd_df
  
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
  punter_rec_kd_df$frames_after_snap <- frame_id
  receiving_kd_list[[frame_id]] <- punter_rec_kd_df
  p1 <- ggplot(punter_kd_df, aes(x = deg_angle, y = dist_to_punter)) + 
    geom_raster(aes(fill=density))+scale_fill_viridis()+xlab("Theta")+ylab("R")+ ggtitle(paste(frame_id, "Frames After Snap"))
  p2 <- ggplot(punter_rec_kd_df, aes(x = deg_angle, y = dist_to_punter)) + 
    geom_raster(aes(fill=density))+scale_fill_viridis()+xlab("Theta")+ylab("R")+ ggtitle(paste(frame_id, "Frames After Snap"))
  pplot_list[[frame_id]] <- p1
  rplot_list[[frame_id]] <- p2
}
punting_kd_df <- rbindlist(punting_kd_list)
receiving_kd_df <- rbindlist(receiving_kd_list)


# punting_kd_list[[1]]
# punting_kd_list[[5]]
# pplot_list[[1]]
# View(punting_kd_df)
# p <- ggplot(receiving_kd_df,aes(deg_angle, dist_to_punter, fill = density))+
#   geom_raster()+scale_fill_viridis(option="inferno")+
#   xlab("x")+ylab("z")
# p +   transition_states(as.factor(frames_after_snap))

library(gganimate)
library(viridis)
p <- ggplot(receiving_kd_df, aes(x = deg_angle, y = dist_to_punter)) +xlab("Theta")+ylab("r")+
  geom_raster(aes(fill=density))+scale_fill_viridis()+
  # labs(title = "Frames After Snap: {frames_after_snap}", x="Theta", y="R")+
  transition_time(as.integer(frames_after_snap))+
  ease_aes('linear')

animate(p, fps=(10), height=400, width=400)

View(receiving_kd_df)
gganimate(p)
  theme(panel.background = element_blank())+
  # labs(title = "Frame After Snap: {frames_after_snap}")+
  transition_states(frames_after_snap)