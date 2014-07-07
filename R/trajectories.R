
#' generate.TrajectoryList
#' 
#' Generates a TrajectoryList object from a LinkedParticleList
#' 
#' @param particlelist A ParticleList object
#' @param provideExtraDetails Logical, currently not used - could be introduced for providing additional info on the trajectories
#' @param ... Arguments to be passed to methods
#' 
#' @return A TrajectoryList object
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
generate.TrajectoryList <- function(particlelist,
                                    provideExtraDetails=FALSE,
                                    ... # parameters for the eventual tracking - is it possible to do so? ask harald!
                                    )
{
  if(is(particlelist,"linkedParticleList"))
  {
    cat("Generating trajectories...\n")
    linkedparticlelist <- particlelist
  } else {
    if(is(particlelist,"ParticleList"))
    {
      cat("Input ParticleList is not a linkedParticleList! \n")
      cat("Performing linking first with some set of default parameters - you might want to change them according to your scenario...\n")
      linkedparticlelist <- link.ParticleList(particlelist,L=26,R=3,epsilon1=0,epsilon2=0,lambda1=1,lambda2=0)
      print(linkedparticlelist)
    }
  }
  
  out <- vector(1,mode="list")
  class(out) <- c("TrajectoryList",class(out))
  
  ntraj <- 0
  linkrange <- ncol(linkedparticlelist[[1]]$nxt)
  
  for(ii in 1:length(linkedparticlelist)) # looping over the frames
  {
    npart <- nrow(linkedparticlelist[[ii]]$particles)
    
    for(ipart in 1:npart) # looping over the particles
    {
      iframe <- ii
      nextOne <- linkedparticlelist[[iframe]]$nxt[ipart,1] # need to tweak the column!
      if(nextOne > 0) # if the particle starts a trajectory, then follow it
      {
        ntraj <- ntraj+1
        traj <- c(linkedparticlelist[[iframe]]$particles[ipart,1],linkedparticlelist[[iframe]]$particles[ipart,2],ntraj,iframe,ipart)
        linkedparticlelist[[iframe]]$link[ipart] <- -2 # instead of -1
        while(nextOne > 0)
        {
          iframe <- iframe + 1
          traj <- rbind(traj,c(linkedparticlelist[[iframe]]$particles[nextOne,1],linkedparticlelist[[iframe]]$particles[nextOne,2],ntraj,iframe,nextOne))
          nextOld <- nextOne
          nextOne <- linkedparticlelist[[iframe]]$nxt[nextOne,1]
          linkedparticlelist[[iframe]]$nxt[nextOld,1] <- -2
        }
        
        out[[ntraj]] <- list()
        
        colnames(traj) <- c("xCoord","yCoord","trajLabel","frame","frameobjectID")
        rownames(traj) <- paste0(ntraj,"_",seq(1:nrow(traj)))
        traj <- as.data.frame(traj)
        out[[ntraj]]$trajectory <- traj
        out[[ntraj]]$npoints <- nrow(traj)
        out[[ntraj]]$nframes <- traj$frame[nrow(traj)]-traj$frame[1] + 1
        out[[ntraj]]$ngaps <- out[[ntraj]]$nframes - out[[ntraj]]$npoints
        out[[ntraj]]$keep <- NA # initialized, then set to 0 or 1
      }
    }
  }
  
  cat("Done!\n")
  return(out)
}








## maybe add this too? IN THE FUNCTION ITSELF!
## dfTrajs <- do.call(rbind.data.frame,liTra)



# intro for this
# imgDimensions <- dim(readImage(i))
# cubeLimits <- list(xlim=c(0,imgDimensions[1]),ylim=c(0,imgDimensions[2]),tlim=c(0,nframes))


#' axesInfo
#' 
#' Auxiliary function to return the dimensions of the field of interest
#'  
#' @param framelist A FrameList object
#' 
#' @return A list object, containing the extremes of the field of interest (x-y-z, where z is time)
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
axesInfo <- function(framelist)
{
  imgDimensions <- dim(framelist[[1]]$image)
  out <- list(xlim=c(0,imgDimensions[1]),ylim=c(0,imgDimensions[2]),tlim=c(0,length(framelist)))
}


#' display.TrajectoryList
#' 
#' Provides a visual representation of a TrajectoryList object
#' 
#' Based on the rgl library, the function extracts the region of interests from the dimensions of an image of the FrameList object,
#' and afterwards plots the x-y-time representation of the identified trajectories
#' 
#' @param trajectorylist A TrajectoryList object
#' @param framelist A FrameList object, used here to identify the limits of the region of interest 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
display.TrajectoryList <- function(trajectorylist,framelist)
{
  trajectoryDataFrame <- do.call(rbind.data.frame,lapply(trajectorylist,function(arg){arg$trajectory}))
  cat("Plotting",length(trajectorylist),"trajectories...\n")
  for (t in 1:max(trajectoryDataFrame$trajLabel))
  {
    singleTraj <- trajectoryDataFrame[which(trajectoryDataFrame$trajLabel==t),]
    plot3d(singleTraj$xCoord, singleTraj$yCoord, singleTraj$frame, col=colours()[singleTraj$trajLabel],type="l",add=T)
    
  }
  cubeLimits <- axesInfo(framelist)
  decorate3d(xlim=cubeLimits$xlim,ylim=cubeLimits$ylim,zlim=cubeLimits$tlim,xlab="",ylab="",zlab="",aspect=T)
  
}




#' paintTrajectory
#' 
#' Creates a FrameList objects containing raw information, combined with the segmented images and the relative trajectory under analysis
#' 
#' @param trajectorylist A TrajectoryList object
#' @param rawframelist A FrameList object with raw images
#' @param preprocessedframelist A FrameList object with preprocessed frames
#' @param trajId Numeric value, the ID of the trajectory 
#' 
#' @return A new FrameList object
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
paintTrajectory <- function(trajectorylist,rawframelist,preprocessedframelist,trajId)
{
  # doing it on one single traj
  
  #   for(i in 1:length(trajectorylist))
  i <- trajId
  {
    currentTraj <- trajectorylist[[i]]$trajectory
    framesIncluded <- currentTraj$frame
    subsetRaw <- subset(rawframelist,framesToKeep=framesIncluded)
    subsetProcessed <- subset(preprocessedframelist,framesIncluded)
    
    out <- vector("list",length(framesIncluded))
    
    for (j in 1:length(out))
    {
      rawimg <- subsetRaw[[j]]$image
      segmimg <- subsetProcessed[[j]]$image
      singleObjectSegm <- segmimg
      singleObjectSegm[segmimg!=currentTraj$frameobjectID[j]] <- 0
      rawWithPaintedObj <- paintObjects(singleObjectSegm,rawimg,col="yellow") #  even more beautiful if i have red cells displayed and contour as YELLOW!
      
      out[[j]]$image <- rawWithPaintedObj 
    }  
  }
  class(out) <- c("FrameList",class(out))
  return(out)

}


## paintTRAJSSSSSS on all, giffed?







#' evaluateTrajectoryList
#' 
#' A pseudo-interactive function to inspect and accept/reject trajectories of a TrajectoryList object
#'  
#' The user gets the trajectories displayed singularly, and for each a keyboard input is required.
#' The slot corresponding to the user feedback is interactively filled in. The TrajectoryList object is then ready for further operations
#' 
#' @param trajectorylist A TrajectoryList object
#' @param rawframelist A FrameList object with raw images
#' @param preprocessedframelist A FrameList object with preprocessed frames
#' 
#' @return A new TrajectoryList object
#'
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
evaluateTrajectoryList <- function(trajectorylist,
                                   rawframelist,
                                   preprocessedframelist
                                   #,trajID
                                   )
{
#   out <- vector("list",length(trajectorylist))
  out <- trajectorylist # initialized as the input object, which is afterwards modified in the corresponding slot
  #   id <- 1 # just for trajId =1 now, then could be actually be done cycling on the trajectories
  for (id in 1:length(trajectorylist))
  {
    cat("Evaluating trajectory",id,"...\n")
    # create painted trajs...
    paintedTraj <- paintTrajectory(trajectorylist,rawframelist,preprocessedframelist,id)
    # display it for checking purposes
#     fullInspection.FrameList(paintedTraj)
    # export it also as gif
    export.FrameList(paintedTraj,nameStub=paste0("evaluation_traj_",id),createGif=TRUE,removeAfterCreatingGif=TRUE)
    
    # "interactive" part, asking the user whether the trajectory is correct
    
    
    # best thing, prompt for something like "did you like the trajectory? :)"
    #   interactive() <- TRUE # does not work-..
    #   if(interactive()==FALSE)  userInput <- readline(prompt="Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")

    
    cat("Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
    userInput <- readLines(n = 1L)
    #   userInput <- readline(prompt="Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
    # if no 0 nor 1, error/do not update, reprompt?
    # otherwise, this becomes the value for the field
    # ... else
    out[[id]]$keep <- as.logical(as.numeric(userInput))
    
    
    ## TODO somehow not expecting the user prompting the value... "no interactive run".. 
  }

  return(out)
}





# 
# linksToTrajectories <- function(imgObjectsList)
# {
#   trajList <- list()
#   ntraj <- 0
#   for(ii in 1:length(imgObjectsList)) # looping over the frames
#   {
#     npart <- nrow(imgObjectsList[[ii]])
#     
#     for(ipart in 1:npart) # looping over the particles
#     {
#       iframe <- ii
#       #       if((ii == 43) &&(ipart==12) ) browser()
#       nextOne <- imgObjectsList[[iframe]]$link[ipart]
#       if(nextOne > 0) # if the particle starts a trajectory, then follow it
#       {
#         ntraj <- ntraj+1
#         traj <- c(imgObjectsList[[iframe]][ipart,1],imgObjectsList[[iframe]][ipart,2],ntraj,iframe)
#         imgObjectsList[[iframe]]$link[ipart] <- -2 # instead of -1
#         while(nextOne > 0)
#         {
#           iframe <- iframe + 1
#           traj <- rbind(traj,c(imgObjectsList[[iframe]][nextOne,1],imgObjectsList[[iframe]][nextOne,2],ntraj,iframe))
#           nextOld <- nextOne
#           nextOne <- imgObjectsList[[iframe]]$link[nextOne]
#           imgObjectsList[[iframe]]$link[nextOld] <- -2
#         }
#         
#         trajList[[ntraj]] <- traj
#         colnames(trajList[[ntraj]]) <- c("xCoord","yCoord","trajLabel","frame")
#         rownames(trajList[[ntraj]]) <- paste0(ntraj,"_",seq(1:nrow(trajList[[ntraj]])))
#         trajList[[ntraj]] <- as.data.frame(trajList[[ntraj]])
#       }
#     }
#   }
#   return(trajList)
# }
# 
# 
# linksToTrajectories_v3 <- function(particleList=imgObjectsList_v3)
# {
#   trajList <- list()
#   ntraj <- 0
#   linkrange <- ncol(particleList[[1]]$nxt) # can be derived from the object
#   
#   
#   for(ii in 1:length(particleList)) # looping over the frames
#   {
#     npart <- nrow(particleList[[ii]]$particles)
#     
#     for(ipart in 1:npart) # looping over the particles
#     {
#       iframe <- ii
#       nextOne <- particleList[[iframe]]$nxt[ipart,1] # need to tweak the column!
#       if(nextOne > 0) # if the particle starts a trajectory, then follow it
#       {
#         ntraj <- ntraj+1
#         traj <- c(particleList[[iframe]]$particles[ipart,1],particleList[[iframe]]$particles[ipart,2],ntraj,iframe)
#         particleList[[iframe]]$link[ipart] <- -2 # instead of -1
#         while(nextOne > 0)
#         {
#           iframe <- iframe + 1
#           traj <- rbind(traj,c(particleList[[iframe]]$particles[nextOne,1],particleList[[iframe]]$particles[nextOne,2],ntraj,iframe))
#           nextOld <- nextOne
#           nextOne <- particleList[[iframe]]$nxt[nextOne,1]
#           particleList[[iframe]]$nxt[nextOld,1] <- -2
#         }
#         
#         trajList[[ntraj]] <- traj
#         colnames(trajList[[ntraj]]) <- c("xCoord","yCoord","trajLabel","frame")
#         rownames(trajList[[ntraj]]) <- paste0(ntraj,"_",seq(1:nrow(trajList[[ntraj]])))
#         trajList[[ntraj]] <- as.data.frame(trajList[[ntraj]])
#       }
#     }
#   }
#   return(trajList)
# }
# ## maybe add this too? IN THE FUNCTION ITSELF!
# ## dfTrajs <- do.call(rbind.data.frame,liTra)
# 
# 
# 
# linksToTrajectories_v5 <- function(particleList=imgObjectsList_v3)
# {
#   # trajSet is now a list of lists!
#   # need some kind of definition for the "class" # see auxiliary.R
#   
#   
#   
#   
#   # still work on the list - singularly-... but the nneed to combine it!
#   trajList <- list()
#   ntraj <- 0
#   linkrange <- ncol(particleList[[1]]$nxt) # can be derived from the object
#   
#   
#   for(ii in 1:length(particleList)) # looping over the frames
#   {
#     npart <- nrow(particleList[[ii]]$particles)
#     
#     for(ipart in 1:npart) # looping over the particles
#     {
#       iframe <- ii
#       nextOne <- particleList[[iframe]]$nxt[ipart,1] # need to tweak the column!
#       if(nextOne > 0) # if the particle starts a trajectory, then follow it
#       {
#         ntraj <- ntraj+1
#         traj <- c(particleList[[iframe]]$particles[ipart,1],particleList[[iframe]]$particles[ipart,2],ntraj,iframe)
#         particleList[[iframe]]$link[ipart] <- -2 # instead of -1
#         while(nextOne > 0)
#         {
#           iframe <- iframe + 1
#           traj <- rbind(traj,c(particleList[[iframe]]$particles[nextOne,1],particleList[[iframe]]$particles[nextOne,2],ntraj,iframe))
#           nextOld <- nextOne
#           nextOne <- particleList[[iframe]]$nxt[nextOne,1]
#           particleList[[iframe]]$nxt[nextOld,1] <- -2
#         }
#         
#         trajList[[ntraj]] <- traj
#         colnames(trajList[[ntraj]]) <- c("xCoord","yCoord","trajLabel","frame")
#         rownames(trajList[[ntraj]]) <- paste0(ntraj,"_",seq(1:nrow(trajList[[ntraj]])))
#         trajList[[ntraj]] <- as.data.frame(trajList[[ntraj]])
#       }
#     }
#   }
#   
#   trajSet <- createTrajectorySet(trajList)
#   
#   return(trajSet)
# }
# ## maybe add this too? IN THE FUNCTION ITSELF!
# ## dfTrajs <- do.call(rbind.data.frame,liTra)




