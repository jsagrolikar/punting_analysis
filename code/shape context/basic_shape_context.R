library(MASS)
library(viridis)
library(tidyverse)
setwd("C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022")
# tracking_sample <- read.csv("tracking2020.csv")
# tracking_sample <- read.csv("2020_reduced.csv")
# tracking_sample$ï..time <- NULL
tracking_sample <- read.csv("2020_reduced.csv")
tracking_sample$ï..time <- NULL
games_df <- read.csv("games.csv")
plays_df <- read.csv("plays.csv")
players_df <- read.csv("players.csv")

left_direction_df <- tracking_sample[tracking_sample$playDirection=="left",]
left_direction_df$y <- 53.3-left_direction_df$y
left_direction_df$x <- 120-left_direction_df$x
tracking_sample <- tracking_sample[tracking_sample$playDirection!="left",]
tracking_sample <- rbind(tracking_sample, left_direction_df)


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
# max(filt_punt_events_df$frames_after_snap)
# 
# aggregate(frame_id~punt_id, filt_punt_events_df)
# sort((filt_punt_events_df$frames_after_snap))
for (frame_id in c(1:25)[1]) {
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
  for (i in 1:1) {
    spec_punt_df <- frame_punt_events_df[frame_punt_events_df$gameId==punt_index_df[i,]$gameId & frame_punt_events_df$playId==punt_index_df[i,]$playId,]
    spec_punt_df$punt_id <- i
    spec_punt_df <- spec_punt_df[order(spec_punt_df$y,spec_punt_df$x),]
    spec_punt_punting_df <- spec_punt_df[spec_punt_df$possessionTeam==spec_punt_df$TeamAbbr,]
    spec_punt_punting_df <- spec_punt_punting_df[order(spec_punt_punting_df$y,spec_punt_punting_df$x),]
    spec_punt_punting_df <- rbind(spec_punt_punting_df[spec_punt_punting_df$position=="P",],spec_punt_punting_df[spec_punt_punting_df$position!="P",])
    spec_punt_receiving_df <- spec_punt_df[spec_punt_df$possessionTeam!=spec_punt_df$TeamAbbr,]
    spec_punt_receiving_df <- spec_punt_receiving_df[order(spec_punt_receiving_df$y,spec_punt_receiving_df$x),]
    spec_punt_receiving_df <- rbind(spec_punt_receiving_df[spec_punt_receiving_df$x==max(spec_punt_receiving_df$x),],spec_punt_receiving_df[spec_punt_receiving_df$x!=max(spec_punt_receiving_df$x),])
    punts_lists <- append(punts_lists, list(spec_punt_df, spec_punt_punting_df, spec_punt_receiving_df))
  }
}

punter_position_df <- spec_punt_punting_df[1,]
other_punting_players_df <- spec_punt_punting_df[-1,]
other_punting_players_df$adj_x <- other_punting_players_df$x-punter_position_df$x
other_punting_players_df$adj_y <- other_punting_players_df$y-punter_position_df$y
other_punting_players_df$rad_angle <- atan(other_punting_players_df$adj_y/other_punting_players_df$adj_x)
other_punting_players_df$deg_angle <-  other_punting_players_df$rad_angle*(180/pi)
other_punting_players_df$dist_to_punter <- sqrt((other_punting_players_df$adj_x^2)+(other_punting_players_df$adj_y^2))

punter_kd <- other_punting_players_df %>%
  with(
    MASS::kde2d(deg_angle, dist_to_punter, n = 101,
                lims = c(
                  scales::expand_range(c(-200,200), .0),
                  scales::expand_range(c(0,75), .0)
                )
    )
  )


punter_kd_df <- punter_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(punter_kd$z))






p1 <- ggplot(punter_kd_df,aes(deg_angle, dist_to_punter, fill = density))+
  geom_raster()+scale_fill_viridis(option="inferno")+
  xlab("x")+ylab("z")+
  theme(panel.background = element_blank())

(cart2pol(diff_df$x, diff_df$y, degrees = FALSE))





for (punting_team_player in c(2:11)) {
  player_df <- spec_punt_punting_df[punting_team_player]
}








