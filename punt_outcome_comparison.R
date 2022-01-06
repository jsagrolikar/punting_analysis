setwd("C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022")
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

# blocked_punts <- tracking_sample[tracking_sample$event=="punt_blocked",]
# blocked_punt_summary <- unique(data.frame(gameId=blocked_punts$gameId, playId=blocked_punts$playId, frameId=blocked_punts$frameId))
# blocked_punts_df <- tracking_sample[tracking_sample$gameId %in% blocked_punt_summary$gameId & tracking_sample$playId %in% blocked_punt_summary$playId,]
# write.csv(exp_df, "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022\\2020_reduced.csv")
# blocked_punts_df$time <- NULL
# exp_df <- rbind(tracking_sample, blocked_punts_df)

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

View(punter_df)

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
# punt_events_df$punter_x <- punt_events_df$punter_x-punt_events_df$los_x
# punt_events_df$punter_y <- punt_events_df$punter_y-punt_events_df$los_y 

punt_blocked_events <- punt_events_df[punt_events_df$event=="punt_blocked",]
punt_downed_events <- punt_events_df[punt_events_df$event=="punt_downed",]
punt_touchback_events <- punt_events_df[punt_events_df$event=="touchback",]
punt_fair_catch_events <- punt_events_df[punt_events_df$event=="fair_catch",]
punt_return_events <- punt_events_df[punt_events_df$event=="tackle" | punt_events_df$event=="touchdown",]


punt_blocked_events_index <- unique(data.frame(gameId=punt_blocked_events$gameId,playId=punt_blocked_events$playId, event="punt_blocked"))
punt_downed_events_index <- unique(data.frame(gameId=punt_downed_events$gameId,playId=punt_downed_events$playId, event="punt_downed"))
punt_touchback_events_index <- unique(data.frame(gameId=punt_touchback_events$gameId,playId=punt_touchback_events$playId, event="touchback"))
punt_fair_catch_events_index <- unique(data.frame(gameId=punt_fair_catch_events$gameId,playId=punt_fair_catch_events$playId, event="fair_catch"))
punt_return_events_index <- unique(data.frame(gameId=punt_return_events$gameId,playId=punt_return_events$playId, event="punt_returned"))


punt_return_football_df <- merge(football_punt_events_df, punt_return_events_index, by=c("gameId","playId"))
punt_received_df <- (punt_return_football_df[punt_return_football_df$event.x=="punt_received" | punt_return_football_df$event.x=="punt_land",])
punt_return_df <- (punt_return_football_df[punt_return_football_df$event.x=="tackle" | punt_return_football_df$event.x=="touchdown",])
punt_receive_return_events <- merge(punt_received_df, punt_return_df, by=c("gameId", "playId"))
punt_receive_return_index <- unique(data.frame(gameId=punt_receive_return_events$gameId,playId=punt_receive_return_events$playId))
football_punt_receive_return_events <- merge(football_punt_events_df, punt_receive_return_index, by=c("gameId","playId"))

punt_return_start_df <- football_punt_receive_return_events[football_punt_receive_return_events$event=="punt_received" | football_punt_receive_return_events$event=="punt_land",]
punt_return_start_df <- punt_return_start_df[order(punt_return_start_df$gameId, punt_return_start_df$playId),]
punt_return_start_df$index <- c(Inf,abs(diff(punt_return_start_df$gameId))+abs(diff(punt_return_start_df$playId)))
punt_return_start_df <- punt_return_start_df[punt_return_start_df$index>0,]
punt_return_end_df <- football_punt_receive_return_events[football_punt_receive_return_events$event=="tackle" | football_punt_receive_return_events$event=="touchdown",]
punt_return_end_df <- punt_return_end_df[order(punt_return_end_df$gameId, punt_return_end_df$playId),]
punt_return_end_df$index <- c(Inf,abs(diff(punt_return_end_df$gameId))+abs(diff(punt_return_end_df$playId)))
punt_return_end_df <- punt_return_end_df[punt_return_end_df$index>0,]
punt_return_end_df$return_length <- abs(punt_return_end_df$x-punt_return_start_df$x)
punt_return_end_df$classification <- NA
punt_return_end_df$classification[punt_return_end_df$return_length<10] <- 1
punt_return_end_df$classification[punt_return_end_df$return_length>=10 & punt_return_end_df$return_length<20] <- 2
punt_return_end_df$classification[punt_return_end_df$return_length>=20 & punt_return_end_df$return_length<30] <- 3
punt_return_end_df$classification[punt_return_end_df$return_length>=30] <- 4
punt_return_categories_index <- unique(data.frame(gameId=punt_return_end_df$gameId, playId=punt_return_end_df$playId, classification=punt_return_end_df$classification))

punt_return_class1_index <- punt_return_categories_index[punt_return_categories_index$classification==1,]
punt_return_class2_index <- punt_return_categories_index[punt_return_categories_index$classification==2,]
punt_return_class3_index <- punt_return_categories_index[punt_return_categories_index$classification==3,]
punt_return_class4_index <- punt_return_categories_index[punt_return_categories_index$classification==4,]

punt_blocked_events_index$event <- NULL
punt_blocked_events_df <- merge(punt_events_df, punt_blocked_events_index, by=c("gameId", "playId"))
punt_downed_events_index$event <- NULL
punt_downed_events_df <- merge(punt_events_df, punt_downed_events_index, by=c("gameId", "playId"))
punt_touchback_events_index$event <- NULL
punt_touchback_events_df <- merge(punt_events_df, punt_touchback_events_index, by=c("gameId", "playId"))
punt_fair_catch_events_index$event <- NULL
punt_fair_catch_events_df <- merge(punt_events_df, punt_fair_catch_events_index, by=c("gameId", "playId"))

punt_return_class1_index$classification <- NULL
punt_return_class1_events_df <- merge(punt_events_df, punt_return_class1_index, by=c("gameId", "playId"))
punt_return_class2_index$classification <- NULL
punt_return_class2_events_df <- merge(punt_events_df, punt_return_class2_index, by=c("gameId", "playId"))
punt_return_class3_index$classification <- NULL
punt_return_class3_events_df <- merge(punt_events_df, punt_return_class3_index, by=c("gameId", "playId"))
punt_return_class4_index$classification <- NULL
punt_return_class4_events_df <- merge(punt_events_df, punt_return_class4_index, by=c("gameId", "playId"))

dir <- "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022/Output/"
filename <- "punt_fair_catch_events"

write.csv(punt_fair_catch_events_df, paste(dir, filename, ".csv", sep=""), row.names = FALSE)
## unsure if we should normalize y to LOS; likely insignificant but should discuss
View(punt_blocked_events_df)
#PUNT BLOCKED
######
punt_blocked_events_filt_df <- punt_blocked_events_df[punt_blocked_events_df$frames_after_snap>=0 & punt_blocked_events_df$frameId<=punt_blocked_events_df$punt_event_frame,]
punt_blocked_events_frame_df <- punt_blocked_events_filt_df[punt_blocked_events_filt_df$frameId==24,]

punt_blocked_events_frame_pt_df <- punt_blocked_events_frame_df[punt_blocked_events_frame_df$team==punt_blocked_events_frame_df$punting_team,]
punt_blocked_events_frame_pt_df <- punt_blocked_events_frame_pt_df[punt_blocked_events_frame_pt_df$position!="P",]
punt_blocked_events_frame_rt_df <- punt_blocked_events_frame_df[punt_blocked_events_frame_df$team!=punt_blocked_events_frame_df$punting_team,]
punt_blocked_events_frame_rt_df <- punt_blocked_events_frame_rt_df[punt_blocked_events_frame_rt_df$position!="P",]

pt_punt_blocked_kd <- punt_blocked_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_blocked_kd_df <- pt_punt_blocked_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_blocked_kd$z))

rt_punt_blocked_kd <- punt_blocked_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_blocked_kd_df <- rt_punt_blocked_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_blocked_kd$z))
######

#PUNT DOWNED
######
punt_downed_events_filt_df <- punt_downed_events_df[punt_downed_events_df$frames_after_snap>=0 & punt_downed_events_df$frameId<=punt_downed_events_df$punt_event_frame,]
punt_downed_events_frame_df <- punt_downed_events_filt_df[punt_downed_events_filt_df$frameId==24,]

punt_downed_events_frame_pt_df <- punt_downed_events_frame_df[punt_downed_events_frame_df$team==punt_downed_events_frame_df$punting_team,]
punt_downed_events_frame_pt_df <- punt_downed_events_frame_pt_df[punt_downed_events_frame_pt_df$position!="P",]
punt_downed_events_frame_rt_df <- punt_downed_events_frame_df[punt_downed_events_frame_df$team!=punt_downed_events_frame_df$punting_team,]
punt_downed_events_frame_rt_df <- punt_downed_events_frame_rt_df[punt_downed_events_frame_rt_df$position!="P",]

pt_punt_downed_kd <- punt_downed_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_downed_kd_df <- pt_punt_downed_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_downed_kd$z))

rt_punt_downed_kd <- punt_downed_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_downed_kd_df <- rt_punt_downed_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_downed_kd$z))
######

#PUNT Touchback
######
punt_touchback_events_filt_df <- punt_touchback_events_df[punt_touchback_events_df$frames_after_snap>=0 & punt_touchback_events_df$frameId<=punt_touchback_events_df$punt_event_frame,]
punt_touchback_events_frame_df <- punt_touchback_events_filt_df[punt_touchback_events_filt_df$frameId==24,]

punt_touchback_events_frame_pt_df <- punt_touchback_events_frame_df[punt_touchback_events_frame_df$team==punt_touchback_events_frame_df$punting_team,]
punt_touchback_events_frame_pt_df <- punt_touchback_events_frame_pt_df[punt_touchback_events_frame_pt_df$position!="P",]
punt_touchback_events_frame_rt_df <- punt_touchback_events_frame_df[punt_touchback_events_frame_df$team!=punt_touchback_events_frame_df$punting_team,]
punt_touchback_events_frame_rt_df <- punt_touchback_events_frame_rt_df[punt_touchback_events_frame_rt_df$position!="P",]

pt_punt_touchback_kd <- punt_touchback_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_touchback_kd_df <- pt_punt_touchback_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_touchback_kd$z))

rt_punt_touchback_kd <- punt_touchback_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_touchback_kd_df <- rt_punt_touchback_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_touchback_kd$z))
######

#PUNT Fair Catch
######
punt_fair_catch_events_filt_df <- punt_fair_catch_events_df[punt_fair_catch_events_df$frames_after_snap>=0 & punt_fair_catch_events_df$frameId<=punt_fair_catch_events_df$punt_event_frame,]
punt_fair_catch_events_frame_df <- punt_fair_catch_events_filt_df[punt_fair_catch_events_filt_df$frameId==24,]

punt_fair_catch_events_frame_pt_df <- punt_fair_catch_events_frame_df[punt_fair_catch_events_frame_df$team==punt_fair_catch_events_frame_df$punting_team,]
punt_fair_catch_events_frame_pt_df <- punt_fair_catch_events_frame_pt_df[punt_fair_catch_events_frame_pt_df$position!="P",]
punt_fair_catch_events_frame_rt_df <- punt_fair_catch_events_frame_df[punt_fair_catch_events_frame_df$team!=punt_fair_catch_events_frame_df$punting_team,]
punt_fair_catch_events_frame_rt_df <- punt_fair_catch_events_frame_rt_df[punt_fair_catch_events_frame_rt_df$position!="P",]

pt_punt_fair_catch_kd <- punt_fair_catch_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_fair_catch_kd_df <- pt_punt_fair_catch_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_fair_catch_kd$z))

rt_punt_fair_catch_kd <- punt_fair_catch_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_fair_catch_kd_df <- rt_punt_fair_catch_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_fair_catch_kd$z))
######

#PUNT Return Class1
######
punt_return_class1_events_filt_df <- punt_return_class1_events_df[punt_return_class1_events_df$frames_after_snap>=0 & punt_return_class1_events_df$frameId<=punt_return_class1_events_df$punt_event_frame,]
punt_return_class1_events_frame_df <- punt_return_class1_events_filt_df[punt_return_class1_events_filt_df$frameId==24,]

punt_return_class1_events_frame_pt_df <- punt_return_class1_events_frame_df[punt_return_class1_events_frame_df$team==punt_return_class1_events_frame_df$punting_team,]
punt_return_class1_events_frame_pt_df <- punt_return_class1_events_frame_pt_df[punt_return_class1_events_frame_pt_df$position!="P",]
punt_return_class1_events_frame_rt_df <- punt_return_class1_events_frame_df[punt_return_class1_events_frame_df$team!=punt_return_class1_events_frame_df$punting_team,]
punt_return_class1_events_frame_rt_df <- punt_return_class1_events_frame_rt_df[punt_return_class1_events_frame_rt_df$position!="P",]

pt_punt_return_class1_kd <- punt_return_class1_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_return_class1_kd_df <- pt_punt_return_class1_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_return_class1_kd$z))

rt_punt_return_class1_kd <- punt_return_class1_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_return_class1_kd_df <- rt_punt_return_class1_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_return_class1_kd$z))
######

#PUNT Return Class2
######
punt_return_class2_events_filt_df <- punt_return_class2_events_df[punt_return_class2_events_df$frames_after_snap>=0 & punt_return_class2_events_df$frameId<=punt_return_class2_events_df$punt_event_frame,]
punt_return_class2_events_frame_df <- punt_return_class2_events_filt_df[punt_return_class2_events_filt_df$frameId==24,]

punt_return_class2_events_frame_pt_df <- punt_return_class2_events_frame_df[punt_return_class2_events_frame_df$team==punt_return_class2_events_frame_df$punting_team,]
punt_return_class2_events_frame_pt_df <- punt_return_class2_events_frame_pt_df[punt_return_class2_events_frame_pt_df$position!="P",]
punt_return_class2_events_frame_rt_df <- punt_return_class2_events_frame_df[punt_return_class2_events_frame_df$team!=punt_return_class2_events_frame_df$punting_team,]
punt_return_class2_events_frame_rt_df <- punt_return_class2_events_frame_rt_df[punt_return_class2_events_frame_rt_df$position!="P",]

pt_punt_return_class2_kd <- punt_return_class2_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_return_class2_kd_df <- pt_punt_return_class2_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_return_class2_kd$z))

rt_punt_return_class2_kd <- punt_return_class2_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_return_class2_kd_df <- rt_punt_return_class2_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_return_class2_kd$z))
######

#PUNT Return Class3
######
punt_return_class3_events_filt_df <- punt_return_class3_events_df[punt_return_class3_events_df$frames_after_snap>=0 & punt_return_class3_events_df$frameId<=punt_return_class3_events_df$punt_event_frame,]
punt_return_class3_events_frame_df <- punt_return_class3_events_filt_df[punt_return_class3_events_filt_df$frameId==24,]

punt_return_class3_events_frame_pt_df <- punt_return_class3_events_frame_df[punt_return_class3_events_frame_df$team==punt_return_class3_events_frame_df$punting_team,]
punt_return_class3_events_frame_pt_df <- punt_return_class3_events_frame_pt_df[punt_return_class3_events_frame_pt_df$position!="P",]
punt_return_class3_events_frame_rt_df <- punt_return_class3_events_frame_df[punt_return_class3_events_frame_df$team!=punt_return_class3_events_frame_df$punting_team,]
punt_return_class3_events_frame_rt_df <- punt_return_class3_events_frame_rt_df[punt_return_class3_events_frame_rt_df$position!="P",]

pt_punt_return_class3_kd <- punt_return_class3_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_return_class3_kd_df <- pt_punt_return_class3_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_return_class3_kd$z))

rt_punt_return_class3_kd <- punt_return_class3_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_return_class3_kd_df <- rt_punt_return_class3_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_return_class3_kd$z))
######

#PUNT Return Class4
######
punt_return_class4_events_filt_df <- punt_return_class4_events_df[punt_return_class4_events_df$frames_after_snap>=0 & punt_return_class4_events_df$frameId<=punt_return_class4_events_df$punt_event_frame,]
punt_return_class4_events_frame_df <- punt_return_class4_events_filt_df[punt_return_class4_events_filt_df$frameId==24,]

punt_return_class4_events_frame_pt_df <- punt_return_class4_events_frame_df[punt_return_class4_events_frame_df$team==punt_return_class4_events_frame_df$punting_team,]
punt_return_class4_events_frame_pt_df <- punt_return_class4_events_frame_pt_df[punt_return_class4_events_frame_pt_df$position!="P",]
punt_return_class4_events_frame_rt_df <- punt_return_class4_events_frame_df[punt_return_class4_events_frame_df$team!=punt_return_class4_events_frame_df$punting_team,]
punt_return_class4_events_frame_rt_df <- punt_return_class4_events_frame_rt_df[punt_return_class4_events_frame_rt_df$position!="P",]

pt_punt_return_class4_kd <- punt_return_class4_events_frame_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
pt_punt_return_class4_kd_df <- pt_punt_return_class4_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(pt_punt_return_class4_kd$z))

rt_punt_return_class4_kd <- punt_return_class4_events_frame_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
rt_punt_return_class4_kd_df <- rt_punt_return_class4_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(rt_punt_return_class4_kd$z))
######


punt_sample_df <- merge(punt_events_df, punt_info_index[sample(c(1:nrow(punt_info_index)), 500),])
punt_sample_df <- punt_sample_df[punt_sample_df$position!="P",]
punt_sample_pt_df <- punt_sample_df[punt_sample_df$team==punt_sample_df$punting_team,]
punt_sample_rt_df <- punt_sample_df[punt_sample_df$team!=punt_sample_df$punting_team,]
mean_punt_pt_kd <- punt_sample_pt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
mean_punt_pt_kd_df <- mean_punt_pt_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(mean_punt_pt_kd$z))
mean_punt_rt_kd <- punt_sample_rt_df %>%
  with(MASS::kde2d(deg_angle, dist_to_punter, n = 100,
                   lims = c(
                     scales::expand_range(c(-180,180), .0),
                     scales::expand_range(c(0,75), .0))))
mean_punt_rt_kd_df <- mean_punt_rt_kd %>%
  .[c("x", "y")] %>%
  cross_df() %>%
  rename("deg_angle" = "x", "dist_to_punter" = "y") %>%
  mutate(density = as.vector(mean_punt_rt_kd$z))

mean_punt_pt_dens <- (mean_punt_pt_kd_df$density/sum(mean_punt_pt_kd_df$density))
mean_punt_rt_dens <- (mean_punt_rt_kd_df$density/sum(mean_punt_rt_kd_df$density))
pt_dens_diff_df <- data.frame(deg_angle=mean_punt_pt_kd_df$deg_angle, dist_to_punter=mean_punt_pt_kd_df$dist_to_punter,density=abs(mean_punt_pt_dens-(pt_punt_blocked_kd_df$density/sum(pt_punt_blocked_kd_df$density)))+
  abs(mean_punt_pt_dens-(pt_punt_downed_kd_df$density/sum(pt_punt_blocked_kd_df$density)))+
  abs(mean_punt_pt_dens-(pt_punt_touchback_kd_df$density/sum(pt_punt_touchback_kd_df$density)))+
  abs(mean_punt_pt_dens-(pt_punt_fair_catch_kd_df$density/sum(pt_punt_fair_catch_kd_df$density)))+
    abs(mean_punt_pt_dens-(pt_punt_return_class1_kd_df$density/sum(pt_punt_return_class1_kd_df$density)))+
    abs(mean_punt_pt_dens-(pt_punt_return_class2_kd_df$density/sum(pt_punt_return_class2_kd_df$density)))+
    abs(mean_punt_pt_dens-(pt_punt_return_class3_kd_df$density/sum(pt_punt_return_class3_kd_df$density)))+
    abs(mean_punt_pt_dens-(pt_punt_return_class4_kd_df$density/sum(pt_punt_return_class4_kd_df$density))))
rt_dens_diff_df <- data.frame(deg_angle=mean_punt_rt_kd_df$deg_angle, dist_to_punter=mean_punt_rt_kd_df$dist_to_punter,density=abs(mean_punt_rt_dens-(rt_punt_blocked_kd_df$density/sum(rt_punt_blocked_kd_df$density)))+
  abs(mean_punt_rt_dens-(rt_punt_downed_kd_df$density/sum(rt_punt_blocked_kd_df$density)))+
  abs(mean_punt_rt_dens-(rt_punt_touchback_kd_df$density/sum(rt_punt_touchback_kd_df$density)))+
  abs(mean_punt_rt_dens-(rt_punt_fair_catch_kd_df$density/sum(rt_punt_fair_catch_kd_df$density)))+
  abs(mean_punt_rt_dens-(rt_punt_return_class1_kd_df$density/sum(rt_punt_return_class1_kd_df$density)))+
  abs(mean_punt_rt_dens-(rt_punt_return_class2_kd_df$density/sum(rt_punt_return_class2_kd_df$density)))+
  abs(mean_punt_rt_dens-(rt_punt_return_class3_kd_df$density/sum(rt_punt_return_class3_kd_df$density)))+
  abs(mean_punt_rt_dens-(rt_punt_return_class4_kd_df$density/sum(rt_punt_return_class4_kd_df$density))))

df <- data.frame(deg_angle=pt_punt_blocked_kd_df$deg_angle,dist_to_punter=pt_punt_blocked_kd_df$dist_to_punter,density=(pt_punt_blocked_kd_df$density-pt_punt_downed_kd_df$density))

punt_events_df_no_punter <- punt_events_df[punt_events_df$position!="P",]
mean_npunt_blocked <- anti_join(punt_events_df_no_punter,punt_blocked_events_index, by=c("gameId", "playId"))
mean_npunt_downed <- anti_join(punt_events_df_no_punter,punt_downed_events_index, by=c("gameId", "playId"))
mean_npunt_touchback <- anti_join(punt_events_df_no_punter,punt_touchback_events_index, by=c("gameId", "playId"))
mean_npunt_fair_catch <- anti_join(punt_events_df_no_punter,punt_fair_catch_events_index, by=c("gameId", "playId"))
mean_npunt_return_class1 <- anti_join(punt_events_df_no_punter,punt_return_class1_index, by=c("gameId", "playId"))
mean_npunt_return_class2 <- anti_join(punt_events_df_no_punter,punt_return_class2_index, by=c("gameId", "playId"))
mean_npunt_return_class3 <- anti_join(punt_events_df_no_punter,punt_return_class3_index, by=c("gameId", "playId"))
mean_npunt_return_class4 <- anti_join(punt_events_df_no_punter,punt_return_class4_index, by=c("gameId", "playId"))



p1 <- ggplot(t_dens_diff_df,aes(deg_angle, dist_to_punter, fill = density))+
  geom_raster()+scale_fill_viridis(option="inferno")+
  xlab("x")+ylab("z")+
  theme(panel.background = element_blank())

sort(unique(punt_blocked_events_df$frameId))




# punt_received_events_index <- unique(data.frame(gameId=punt_received_df$gameId,playId=punt_received_df$playId, event="punt_received"))
# punt_receive_return_football_df <- merge(punt_return_football_df, punt_received_events_index, by=c("gameId","playId"))
# punt_received_df[order(punt_received_df$gameId, punt_received_df$playId),]$x-punt_receive_return_football_df[order(punt_receive_return_football_df$gameId, punt_receive_return_football_df$playId),]
# 
# 
# 
# punt_tackle_df <- punt_return_football_df[punt_return_football_df$event.x=="tackle" | punt_return_football_df$event.x=="touchdown",]
# unique(punt_return_football_df$event.x)
# punt_blocked_events_filt_df$adj_x <- punt_blocked_events_filt_df$x-punt_blocked_events_filt_df$punter_x
# punt_blocked_events_filt_df$adj_y <- punt_blocked_events_filt_df$y-punt_blocked_events_filt_df$punter_y
# punt_blocked_events_filt_df$rad_angle <- atan(punt_blocked_events_filt_df$adj_y/punt_blocked_events_filt_df$adj_x)
# punt_blocked_events_filt_df$deg_angle <-  punt_blocked_events_filt_df$rad_angle*(180/pi)
# punt_blocked_events_filt_df$dist_to_punter <- sqrt((punt_blocked_events_filt_df$adj_x^2)+(punt_blocked_events_filt_df$adj_y^2))

