
runFrames <- function(frameStart,frameEnd,movementDirection="leftToRight")
{
  # initializing the labels for tracking
  iamtracking <- imgObjectsDF
  
  iamtracking[iamtracking$frame==1,]$label <- 1:sum(iamtracking$frame==1)
  iamtracking[iamtracking$frame==1,]$processed <- TRUE
  iamtracking[iamtracking$frame==1,]$startOfTrack <- TRUE
  
  maximumPixelDistance <- 60
  maximumYdisplacement <- 10
  maximumBackwardsDisplacement <- 5
  # as we can exploit the fact that cells are flowing in one direction (in our case, to the left)
  
  
  # for(f in 1:(length(candidate_feats_green)-1)) # to the last of the frames actually (-1)
  # for(f in 1:10) # to the last of the frames actually (-1)
  for(f in frameStart:(frameEnd-1)) # to the last of the frames actually (-1)
  {
    cat("FRAME",f,"under analysis\n----------------------\n")
    
    cat("errorPoint A\n")
    # define the frames to work on
    currentFrame <- iamtracking[iamtracking$frame==f,]
    nextFrame <- iamtracking[iamtracking$frame==f+1,]
    
    # initialize the distance matrices
    eucliDist <- matrix(NA,nrow=nrow(currentFrame),ncol=nrow(nextFrame))
    dists2 <- eucliDist
    ydispl <- eucliDist
    xdispl <- eucliDist
    backdispl <- eucliDist
    
    
    cat("errorPoint A2\n")
    #     browser()
    # calculate distances & co
    for (i in 1:nrow(currentFrame))
    {
      for (j in 1:nrow(nextFrame))
      {
        #         cat(j,"\t")
        frameMeasurements <- calcDist2(currentFrame[i,],nextFrame[j,],movementDirection=movementDirection)
        cat()
        dists2[i,j] <- frameMeasurements[[1]]
        xdispl[i,j] <- frameMeasurements[[2]]
        ydispl[i,j] <- frameMeasurements[[3]]
        backdispl[i,j] <- frameMeasurements[[4]]
      }
    }
    cat("errorPoint B\n")
    
    assignedCounter <- 0
    # scrolling through dists2... i.e. checking row by row from the current frame to the next
    for (i in 1:nrow(currentFrame))
    {
      # the track is the one of the row under analysis
      currentTrack <- currentFrame$label[i]
      # order the distances with the others
      sorter <- order(dists2[i,])
      sortedDistances <- dists2[i,][sorter]
      corresponding_yDisplacement <- ydispl[i,][sorter]
      corresponding_backwardsDisplacement <- backdispl[i,][sorter]
      alreadyUsed <- which(iamtracking[iamtracking$frame==f+1,][sorter,]$processed==TRUE)
      
      # shrinking the possibilities...
      discarded <- unique(c(which(corresponding_yDisplacement > maximumYdisplacement),
                            which(corresponding_backwardsDisplacement > maximumBackwardsDisplacement),
                            alreadyUsed))
      #     cat("bp1",i,"\n")#;browser()
      if(!is.null(discarded))
      {
        candidates <- sortedDistances[-discarded]
      } else
        candidates <- sortedDistances
      #     cat(candidates)
      
      if(length(candidates)>0)
      {
        if (candidates[1] <= maximumPixelDistance)
        {
          #         iamtracking$label[iamtracking$frame==f+1,][which(dists2[i,]==candidates[1])] <- currentTrack
          ## THIS is the assignment
          #         browser()
          iamtracking[iamtracking$frame==f+1,][sorter,][-discarded,]$label[1]  <- currentTrack
          iamtracking[iamtracking$frame==f+1,][sorter,][-discarded,]$processed[1]  <- TRUE
          assignedCounter <- assignedCounter + 1
          # this that follows is just an update
          #         yetToBeAssigned <- yetToBeAssigned[-which(row.names(yetToBeAssigned)==row.names(iamtracking[iamtracking$frame==f+1,][sorter,][-discarded,])),]
          #         labeledToBeMatched <- labeledToBeMatched[-which(labeledToBeMatched$label==currentTrack),]
          #       
        } else # noone is in the nearbies
        {
          #         cat("   ------    noone in the nearbies -> track broken\n")
        }
      } else # no candidates are there - because no object respects the imposed rules
      {
        #       cat("no candidates found!")
        # no candidates are left, the track is broken there
        #       cat("track broken\n")
      }
    }  
    cat("errorPoint C\n")
    
    cat("No more candidates left, moving on to next frame!\n\n")
    # but before this, assign tracks to the nextframe incrementally.
    largestTrackIndex <- max(iamtracking$label,na.rm=TRUE)
    startFrom <- largestTrackIndex + 1
    proceedForNSteps <- nrow(nextFrame)-assignedCounter # for the ones that are there, but no match was found
    wereNAs <- is.na(iamtracking$label[iamtracking$frame==f+1])
    if(proceedForNSteps > 0)
    {
      iamtracking$label[iamtracking$frame==f+1][wereNAs] <- seq(from=startFrom,length.out=proceedForNSteps)
      iamtracking$processed[iamtracking$frame==f+1][wereNAs] <- TRUE
      iamtracking$startOfTrack[iamtracking$frame==f+1][wereNAs] <- TRUE
    }
  }
  return(iamtracking)
}
# iamtracking <- runFrames(frameStart=1,frameEnd=100)
# library("rgl")
# plot3d(iamtracking$cell.0.m.cx, iamtracking$cell.0.m.cy, iamtracking$frame, col=iamtracking$label)
