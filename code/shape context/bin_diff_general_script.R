pt_list <- list()
rt_list <- list()
for (selected_frame in c(1,5,10,15,20)) {
#PUNT BLOCKED
######
punt_blocked_events_filt_df <- punt_blocked_events_df[punt_blocked_events_df$frames_after_snap>=0 & punt_blocked_events_df$frameId<=punt_blocked_events_df$punt_event_frame,]
punt_blocked_events_frame_df <- punt_blocked_events_filt_df[punt_blocked_events_filt_df$frames_after_snap==selected_frame,]

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
punt_downed_events_frame_df <- punt_downed_events_filt_df[punt_downed_events_filt_df$frames_after_snap==selected_frame,]

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
punt_touchback_events_frame_df <- punt_touchback_events_filt_df[punt_touchback_events_filt_df$frames_after_snap==selected_frame,]

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
punt_fair_catch_events_frame_df <- punt_fair_catch_events_filt_df[punt_fair_catch_events_filt_df$frames_after_snap==selected_frame,]

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
punt_return_class1_events_frame_df <- punt_return_class1_events_filt_df[punt_return_class1_events_filt_df$frames_after_snap==selected_frame,]

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
punt_return_class2_events_frame_df <- punt_return_class2_events_filt_df[punt_return_class2_events_filt_df$frames_after_snap==selected_frame,]

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
punt_return_class3_events_frame_df <- punt_return_class3_events_filt_df[punt_return_class3_events_filt_df$frames_after_snap==selected_frame,]

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
punt_return_class4_events_frame_df <- punt_return_class4_events_filt_df[punt_return_class4_events_filt_df$frames_after_snap==selected_frame,]

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


punt_sample_df <- punt_events_df
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
pt_dens_diff_df <- data.frame(deg_angle=mean_punt_pt_kd_df$deg_angle, dist_to_punter=mean_punt_pt_kd_df$dist_to_punter, frames_after_snap=selected_frame,
                              density=abs(mean_punt_pt_dens-(pt_punt_blocked_kd_df$density/sum(pt_punt_blocked_kd_df$density)))+
                                abs(mean_punt_pt_dens-(pt_punt_downed_kd_df$density/sum(pt_punt_blocked_kd_df$density)))+
                                abs(mean_punt_pt_dens-(pt_punt_touchback_kd_df$density/sum(pt_punt_touchback_kd_df$density)))+
                                abs(mean_punt_pt_dens-(pt_punt_fair_catch_kd_df$density/sum(pt_punt_fair_catch_kd_df$density)))+
                                abs(mean_punt_pt_dens-(pt_punt_return_class1_kd_df$density/sum(pt_punt_return_class1_kd_df$density)))+
                                abs(mean_punt_pt_dens-(pt_punt_return_class2_kd_df$density/sum(pt_punt_return_class2_kd_df$density)))+
                                abs(mean_punt_pt_dens-(pt_punt_return_class3_kd_df$density/sum(pt_punt_return_class3_kd_df$density)))+
                                abs(mean_punt_pt_dens-(pt_punt_return_class4_kd_df$density/sum(pt_punt_return_class4_kd_df$density))))
rt_dens_diff_df <- data.frame(deg_angle=mean_punt_rt_kd_df$deg_angle, dist_to_punter=mean_punt_rt_kd_df$dist_to_punter, frames_after_snap=selected_frame,
                              density=abs(mean_punt_rt_dens-(rt_punt_blocked_kd_df$density/sum(rt_punt_blocked_kd_df$density)))+
                                abs(mean_punt_rt_dens-(rt_punt_downed_kd_df$density/sum(rt_punt_blocked_kd_df$density)))+
                                abs(mean_punt_rt_dens-(rt_punt_touchback_kd_df$density/sum(rt_punt_touchback_kd_df$density)))+
                                abs(mean_punt_rt_dens-(rt_punt_fair_catch_kd_df$density/sum(rt_punt_fair_catch_kd_df$density)))+
                                abs(mean_punt_rt_dens-(rt_punt_return_class1_kd_df$density/sum(rt_punt_return_class1_kd_df$density)))+
                                abs(mean_punt_rt_dens-(rt_punt_return_class2_kd_df$density/sum(rt_punt_return_class2_kd_df$density)))+
                                abs(mean_punt_rt_dens-(rt_punt_return_class3_kd_df$density/sum(rt_punt_return_class3_kd_df$density)))+
                                abs(mean_punt_rt_dens-(rt_punt_return_class4_kd_df$density/sum(rt_punt_return_class4_kd_df$density))))

pt_list[[selected_frame]] <- pt_dens_diff_df
rt_list[[selected_frame]] <- rt_dens_diff_df
print(selected_frame)
}

for (i in c(1:24)) {
  dir <- "C:/Users/pauli/OneDrive - The University of Chicago/Documents/NFL Research/Big Data Bowl 2022/nfl-big-data-bowl-2022/Output/"
  filename <- paste("frame", as.character(i), "shape_context_weights_pt.csv", sep="")
  filename2 <- paste("frame", as.character(i), "shape_context_weights_rt.csv", sep="")
  write.csv(pt_list[[i]], paste(dir, filename, sep=""), row.names = FALSE)
  write.csv(rt_list[[i]], paste(dir, filename, sep=""), row.names = FALSE)
}
