library(gganimate)
library(cowplot)
setwd("C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022")
tracking_sample <- read.csv("sampleplay.csv")
games_df <- read.csv("games.csv")
plays_df <- read.csv("plays.csv")
players_df <- read.csv("players.csv")
game_play_id <- "2020110808/2859"
inp_gameId <- strsplit(game_play_id, split="/")[[1]][1]
inp_playId <- strsplit(game_play_id, split="/")[[1]][2]
tracking_sample <- tracking_sample[tracking_sample$gameId==inp_gameId & tracking_sample$playId==inp_playId,]

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

## gets football location data and makes a df with LOS information
ball_snap_punt_df <- punt_events_df[punt_events_df$event=="ball_snap",]
ball_snap_punt_info_index <- unique(data.frame(gameId=ball_snap_punt_df$gameId, playId=ball_snap_punt_df$playId, ball_snap_event_frame=ball_snap_punt_df$frameId))
punt_info_index <- merge(punt_info_index, ball_snap_punt_info_index, by=c("gameId","playId"))
punt_events_df <- merge(tracking_sample, punt_info_index, by=c("gameId", "playId"))

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
punt_events_df$x <-punt_events_df$x+100
punt_events_df$y <-punt_events_df$y+160/6

example.play <- punt_events_df[punt_events_df$frames_after_snap>=0 & punt_events_df$frames_after_snap<=(punt_events_df$punt_event_frame-punt_events_df$ball_snap_event_frame),]

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

scale <- (((max(example.play$jaccardradius)-min(example.play$jaccardradius))/283)*0:283+1)*5
#scale <- sort(example.play$jaccardradius)*10
animate.play <- ggplot() +
  scale_size_manual(values = c(scale), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  # scale_fill_manual(values = c("#e31837", "#0073cf", "#0073cf"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#0073cf", "#c60c30"), guide = FALSE) + 
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                      fill = team, colour = team, size = as.factor(jaccardradius)), alpha = 0.7) + 
  geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + theme(legend.position = "bottom")+
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frameId))
animate(animate.play, fps = 10, nframe = play.length.ex)