linksToTrajectories <- function(imgObjectsList)
{
  trajList <- list()
  ntraj <- 0
  for(ii in 1:length(imgObjectsList)) # looping over the frames
  {
    npart <- nrow(imgObjectsList[[ii]])
    
    for(ipart in 1:npart) # looping over the particles
    {
      iframe <- ii
      #       if((ii == 43) &&(ipart==12) ) browser()
      nextOne <- imgObjectsList[[iframe]]$link[ipart]
      if(nextOne > 0) # if the particle starts a trajectory, then follow it
      {
        ntraj <- ntraj+1
        traj <- c(imgObjectsList[[iframe]][ipart,1],imgObjectsList[[iframe]][ipart,2],ntraj,iframe)
        imgObjectsList[[iframe]]$link[ipart] <- -2 # instead of -1
        while(nextOne > 0)
        {
          iframe <- iframe + 1
          traj <- rbind(traj,c(imgObjectsList[[iframe]][nextOne,1],imgObjectsList[[iframe]][nextOne,2],ntraj,iframe))
          nextOld <- nextOne
          nextOne <- imgObjectsList[[iframe]]$link[nextOne]
          imgObjectsList[[iframe]]$link[nextOld] <- -2
        }
        
        trajList[[ntraj]] <- traj
        colnames(trajList[[ntraj]]) <- c("xCoord","yCoord","trajLabel","frame")
        rownames(trajList[[ntraj]]) <- paste0(ntraj,"_",seq(1:nrow(trajList[[ntraj]])))
        trajList[[ntraj]] <- as.data.frame(trajList[[ntraj]])
      }
    }
  }
  return(trajList)
}


linksToTrajectories_v3 <- function(particleList=imgObjectsList_v3)
{
  trajList <- list()
  ntraj <- 0
  linkrange <- ncol(particleList[[1]]$nxt) # can be derived from the object
  
  
  for(ii in 1:length(particleList)) # looping over the frames
  {
    npart <- nrow(particleList[[ii]]$particles)
    
    for(ipart in 1:npart) # looping over the particles
    {
      iframe <- ii
      nextOne <- particleList[[iframe]]$nxt[ipart,1] # need to tweak the column!
      if(nextOne > 0) # if the particle starts a trajectory, then follow it
      {
        ntraj <- ntraj+1
        traj <- c(particleList[[iframe]]$particles[ipart,1],particleList[[iframe]]$particles[ipart,2],ntraj,iframe)
        particleList[[iframe]]$link[ipart] <- -2 # instead of -1
        while(nextOne > 0)
        {
          iframe <- iframe + 1
          traj <- rbind(traj,c(particleList[[iframe]]$particles[nextOne,1],particleList[[iframe]]$particles[nextOne,2],ntraj,iframe))
          nextOld <- nextOne
          nextOne <- particleList[[iframe]]$nxt[nextOne,1]
          particleList[[iframe]]$nxt[nextOld,1] <- -2
        }
        
        trajList[[ntraj]] <- traj
        colnames(trajList[[ntraj]]) <- c("xCoord","yCoord","trajLabel","frame")
        rownames(trajList[[ntraj]]) <- paste0(ntraj,"_",seq(1:nrow(trajList[[ntraj]])))
        trajList[[ntraj]] <- as.data.frame(trajList[[ntraj]])
      }
    }
  }
  return(trajList)
}
## maybe add this too? IN THE FUNCTION ITSELF!
## dfTrajs <- do.call(rbind.data.frame,liTra)



# intro for this
# imgDimensions <- dim(readImage(i))
# cubeLimits <- list(xlim=c(0,imgDimensions[1]),ylim=c(0,imgDimensions[2]),tlim=c(0,nframes))

showTrajectories <- function(trajectoryList,cubeLimits)
{
  trajectoryDataFrame <- do.call(rbind.data.frame, trajectoryList)
  cat("Plotting",max(trajectoryDataFrame$trajLabel),"trajectories...\n")
  for (t in 1:max(trajectoryDataFrame$trajLabel))
  {
    singleTraj <- trajectoryDataFrame[which(trajectoryDataFrame$trajLabel==t),]
    plot3d(singleTraj$xCoord, singleTraj$yCoord, singleTraj$frame, col=colours()[singleTraj$trajLabel],type="l",add=T)
    
  }
  decorate3d(xlim=cubeLimits$xlim,ylim=cubeLimits$ylim,zlim=cubeLimits$tlim,xlab="",ylab="",zlab="",aspect=T)
  
}



linksToTrajectories_v5 <- function(particleList=imgObjectsList_v3)
{
  # trajSet is now a list of lists!
  # need some kind of definition for the "class" # see auxiliary.R
  
  
  
  
  # still work on the list - singularly-... but the nneed to combine it!
  trajList <- list()
  ntraj <- 0
  linkrange <- ncol(particleList[[1]]$nxt) # can be derived from the object
  
  
  for(ii in 1:length(particleList)) # looping over the frames
  {
    npart <- nrow(particleList[[ii]]$particles)
    
    for(ipart in 1:npart) # looping over the particles
    {
      iframe <- ii
      nextOne <- particleList[[iframe]]$nxt[ipart,1] # need to tweak the column!
      if(nextOne > 0) # if the particle starts a trajectory, then follow it
      {
        ntraj <- ntraj+1
        traj <- c(particleList[[iframe]]$particles[ipart,1],particleList[[iframe]]$particles[ipart,2],ntraj,iframe)
        particleList[[iframe]]$link[ipart] <- -2 # instead of -1
        while(nextOne > 0)
        {
          iframe <- iframe + 1
          traj <- rbind(traj,c(particleList[[iframe]]$particles[nextOne,1],particleList[[iframe]]$particles[nextOne,2],ntraj,iframe))
          nextOld <- nextOne
          nextOne <- particleList[[iframe]]$nxt[nextOne,1]
          particleList[[iframe]]$nxt[nextOld,1] <- -2
        }
        
        trajList[[ntraj]] <- traj
        colnames(trajList[[ntraj]]) <- c("xCoord","yCoord","trajLabel","frame")
        rownames(trajList[[ntraj]]) <- paste0(ntraj,"_",seq(1:nrow(trajList[[ntraj]])))
        trajList[[ntraj]] <- as.data.frame(trajList[[ntraj]])
      }
    }
  }
  
  trajSet <- createTrajectorySet(trajList)
  
  return(trajSet)
}
## maybe add this too? IN THE FUNCTION ITSELF!
## dfTrajs <- do.call(rbind.data.frame,liTra)





# trajectoryList <- liTra
# imgRepository <- "/Volumes/flow/test_28_03_2014/cutout2/"
# foundImgs <- listImages(imgRepository)

inspectTrajectories <- function(trajectoryList,
                                imgRepository)
{
  # work on the same object trajectoryList, modifying some fields ad hoc/adding some dedicated!
  
  
  
  # do it for on trajectory, e.g.
  trajNr <- 36
  particleRadius <- 3
  particleColor <- "white" # or choose a nice vector for them!
  # it is loopable :)
  
  acquiredImages <- listImages(imgRepository)
  
  currentTraj <- trajectoryList[[trajNr]]
  nrPoints <- nrow(currentTraj)
  nrFrames <- currentTraj$frame[nrow(currentTraj)]-currentTraj$frame[1] + 1
  nrGaps <- nrFrames - nrPoints
  cat("Displaying a trajectory of",nrPoints,"points over",nrFrames,"frames ---",nrGaps,"gaps detected!\n")
  imgsForTheTrajectory <- acquiredImages[currentTraj$frame]
  
  # for each point of the trajectory
  for(p in 1:nrow(currentTraj))
  {
    img <- readImage(imgsForTheTrajectory[p])
    imgWithCircle <- drawCircle(img,x=currentTraj$xCoord[p],y=currentTraj$yCoord[p],radius=particleRadius,col=particleColor)
    display(imgWithCircle,method="raster") # or save?
    # best thing, prompt for something like "did you like the trajectory? :)"
  }
  
  
  
}




# trajectorySet <- liTra2
# imgRepository <- "/Volumes/flow/test_28_03_2014/cutout2/"
# foundImgs <- listImages(imgRepository)

inspectTrajectorySet <- function(trajectorySet,
                                 imgRepository)
{
  # work on the object trajectorySet, some fields ad hoc/adding some dedicated!
  
  # do it for on trajectory, e.g.
  trajNr <- 36
  particleRadius <- 3
  particleColor <- "white" # or choose a nice vector for them!
  # it is loopable :)
  acquiredImages <- listImages(imgRepository)
  
  currentTraj <- trajectorySet[[trajNr]]$trajectory
  # old
  #   nrPoints <- nrow(currentTraj)
  #   nrFrames <- currentTraj$frame[nrow(currentTraj)]-currentTraj$frame[1] + 1
  #   nrGaps <- nrFrames - nrPoints
  # using the dedicated fields
  nrPoints <- trajectorySet[[trajNr]]$npoints
  nrFrames <- trajectorySet[[trajNr]]$nframes
  nrGaps <- trajectorySet[[trajNr]]$ngaps
  
  cat("Displaying a trajectory of",nrPoints,"points over",nrFrames,"frames ---",nrGaps,"gaps detected!\n")
  imgsForTheTrajectory <- acquiredImages[currentTraj$frame]
  
  # for each point of the trajectory
  for(p in 1:nrow(currentTraj))
  {
    img <- readImage(imgsForTheTrajectory[p])
    imgWithCircle <- drawCircle(img,x=currentTraj$xCoord[p],y=currentTraj$yCoord[p],radius=particleRadius,col=particleColor)
    display(imgWithCircle,method="raster") # or save?
    
  }
  
  
  
  # best thing, prompt for something like "did you like the trajectory? :)"
  #   interactive() <- TRUE # does not work-..
#   if(interactive()==FALSE)  userInput <- readline(prompt="Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
  #   
  cat("Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
  userInput <- readLines(n = 1L)
  #   userInput <- readline(prompt="Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
  # if no 0 nor 1, error/do not update, reprompt?
  # otherwise, this becomes the value for the field
  # ... else
  trajectorySet[[trajNr]]$keep <- as.logical(as.numeric(userInput))
  
  
  ## TODO somehow not expecting the user prompting the value... "no interactive run"..
  
  
  
  
  
  modTrajSet <- trajectorySet
  return(modTrajSet)
}


