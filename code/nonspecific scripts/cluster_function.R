## clusters sample into 1 group step-by-step
## currently prints distance matrices as we go

  ## sets sample size
  n <- (nrow(frame_punt_events_df))^(1/2) - 1
  
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
  
  ## NOT FINISHED - matrix to display how clusters form more elegantly
  cluster_progression <- matrix(nrow=n, ncol=n)
  for (i in 1:n) {
    for (j in 1:n) {
      cluster_progression[i, j] <- j
    }
  }
  cluster_progression <- t(apply(cluster_progression, 1, paste))
  counter <- 2
  
  distancematrices <- list()
  
  ## loops until only two punts left
  while (n >= 2) {
    
    ## calculates distance matrix for punts left in the list 
    dist_mat <- matrix(nrow=n, ncol=n)
    for (i in 1:n) {
      for (j in 1:n) {
        total_cluster_distance <- (sqrt(((punts_lists[[(3*i)-1]]$x-punts_lists[[(3*j)-1]]$x)^2)+((punts_lists[[(3*i)-1]]$y-punts_lists[[(3*j)-1]]$y)^2)))+
          (sqrt(((punts_lists[[(3*i)-0]]$x-punts_lists[[(3*j)-0]]$x)^2)+((punts_lists[[(3*i)-0]]$y-punts_lists[[(3*j)-0]]$y)^2)))
        summed_cluster_distance <- (unname(tapply(total_cluster_distance, (seq_along(total_cluster_distance)-1) %/% 11, sum)))
        dist_mat[i, j] <- summed_cluster_distance
      }
    }
    ## removes zeroes
    dist_mat[lower.tri(dist_mat, diag=TRUE)] <- NA
    distancematrices <- append(distancematrices, list(dist_mat))
    print(dist_mat)
    
    ## finds minimum value, with row and column
    ## this logic is horrible, idk why elegant stuff isn't working
    minindex <- which.min(dist_mat)
    mincol <- (minindex %/% n) + 1
    minrow <- minindex %% n
    
    ## removes the higher indexed punt and amalgamates it to the lower
    if (minrow < mincol) {
      
      ## updates cluster progression matrix (bad logic atm)
      cluster_progression[counter, minrow] <- paste(cluster_progression[counter, mincol], cluster_progression[counter, minrow], sep=", ")
      cluster_progression[counter, mincol] <- NA
      counter <- counter + 1
      
      ## takes mean of two closest punts and sets it as new value for the first punt
      ## e.g. if punt 6 and 12 are closest, takes the means of x and y and updates punt 6 with new values
      punts_lists[[3*minrow]]$x <- (1/2)*(punts_lists[[3*minrow]]$x + punts_lists[[3*mincol]]$x)
      punts_lists[[3*minrow]]$y <- (1/2)*(punts_lists[[3*minrow]]$y + punts_lists[[3*mincol]]$y)
      punts_lists[[3*minrow-1]]$x <- (1/2)*(punts_lists[[3*minrow]]$x + punts_lists[[3*mincol-1]]$x)
      punts_lists[[3*minrow-1]]$y <- (1/2)*(punts_lists[[3*minrow]]$y + punts_lists[[3*mincol-1]]$y)
      punts_lists[[3*minrow-2]]$x <- (1/2)*(punts_lists[[3*minrow]]$x + punts_lists[[3*mincol-2]]$x)
      punts_lists[[3*minrow-2]]$y <- (1/2)*(punts_lists[[3*minrow]]$y + punts_lists[[3*mincol-2]]$y)
      
      ## removes latter punt (12 in example above)
      punts_lists <- punts_lists[-(3*mincol)]
      punts_lists <- punts_lists[-(3*mincol-1)]
      punts_lists <- punts_lists[-(3*mincol-2)]
      
    } else if (mincol > minrow) {
      
      cluster_progression[counter, mincol] <- paste(cluster_progression[counter, mincol], cluster_progression[counter, minrow], sep=", ")
      cluster_progression[counter, minrow] <- NA
      counter <- counter + 1
      
      punts_lists[[3*mincol]]$x <- (1/2)*(punts_lists[[3*mincol]]$x + punts_lists[[3*minrow]]$x)
      punts_lists[[3*mincol]]$y <- (1/2)*(punts_lists[[3*mincol]]$y + punts_lists[[3*minrow]]$y)
      punts_lists[[3*mincol-1]]$x <- (1/2)*(punts_lists[[3*mincol-1]]$x + punts_lists[[3*minrow-1]]$x)
      punts_lists[[3*mincol-1]]$y <- (1/2)*(punts_lists[[3*mincol-1]]$y + punts_lists[[3*minrow-1]]$y)
      punts_lists[[3*mincol-2]]$x <- (1/2)*(punts_lists[[3*mincol-2]]$x + punts_lists[[3*minrow-2]]$x)
      punts_lists[[3*mincol-2]]$y <- (1/2)*(punts_lists[[3*mincol-2]]$y + punts_lists[[3*minrow-2]]$y)
      
      punts_lists <- punts_lists[-(3*minrow)]
      punts_lists <- punts_lists[-(3*minrow-1)]
      punts_lists <- punts_lists[-(3*minrow-2)]
      
    }
    
    ## for the while loop
    n <- n-1
  }

