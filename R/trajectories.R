
#' trajectories
#' 
#' Generates a TrajectoryList object from a (Linked)ParticleList
#' 
#' @param particlelist A (Linked)ParticleList object
#' @param provideExtraDetails Logical, currently not used - could be introduced for providing additional info on the trajectories
#' @param ... Arguments to be passed to methods
#' 
#' @return A TrajectoryList object
#' 
#' @examples
#' load(file.path(system.file("extra", package="flowcatchR"),"candidate.platelets.RData"))
#' platelets.trajectories <- trajectories(candidate.platelets)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
trajectories <- function(particlelist,
                         provideExtraDetails=FALSE,
                         ...) # parameters for the eventual tracking - is it possible to do so? ask harald!
{
  if(is(particlelist,"LinkedParticleList"))
  {
    cat("Generating trajectories...\n")
    linkedparticlelist <- particlelist
  } else {
    if(is(particlelist,"ParticleList"))
    {
      cat("Input ParticleList is not a LinkedParticleList! \n")
      cat("Performing linking first with some set of default parameters - you might want to change them according to your scenario...\n")
      linkedparticlelist <- link.particles(particlelist,L=26,R=3,epsilon1=0,epsilon2=0,lambda1=1,lambda2=0)
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
        out[[ntraj]]$ID <- ntraj
      }
    }
  }
  
  cat("Done!\n")
  return(out)
}



#' axesInfo
#' 
#' Auxiliary function to return the dimensions of the field of interest
#'  
#' @param framelist A FrameList object
#' 
#' @return A list object, containing the extremes of the field of interest (x-y-z, where z is time)
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
axesInfo <- function(framelist)
{
  imgDimensions <- dim(framelist[[1]]$image)
  out <- list(xlim=c(0,imgDimensions[1]),ylim=c(0,imgDimensions[2]),tlim=c(0,length(framelist)))
}


#' plot.TrajectoryList
#' 
#' Provides a visual representation of a TrajectoryList object
#' 
#' Based on the rgl library, the function extracts the region of interests from the dimensions of an image of the FrameList object,
#' and afterwards plots the x-y-time representation of the identified trajectories
#' 
#' @param x A TrajectoryList object
#' @param framelist A FrameList object, used here to identify the limits of the region of interest 
#' @param ... Arguments to be passed to methods
#'
#' @method plot TrajectoryList
#' 
#' @examples
#' load(file.path(system.file("extra", package="flowcatchR"),"MesenteriumSubset.RData"))
#' load(file.path(system.file("extra", package="flowcatchR"),"candidate.platelets.RData"))
#' platelets.trajectories <- trajectories(candidate.platelets)
#' plot(platelets.trajectories,MesenteriumSubset)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
plot.TrajectoryList <- function(x,framelist,...)
{
  trajectoryDataFrame <- do.call(rbind.data.frame,lapply(x,function(arg){arg$trajectory}))
  cat("Plotting",length(x),"trajectories...\n")
  colcols <- rep(colorRamps::primary.colors(40,steps=10,F),6)
  for (i in 1:max(trajectoryDataFrame$trajLabel))
  {
    singleTraj <- trajectoryDataFrame[which(trajectoryDataFrame$trajLabel==i),]
    plot3d(singleTraj$xCoord, singleTraj$yCoord, singleTraj$frame, col=colcols[singleTraj$trajLabel],type="l",lwd = 3,add=T)
    
  }
  cubeLimits <- axesInfo(framelist)
#   
  decorate3d(xlim=cubeLimits$xlim,ylim=cubeLimits$ylim,zlim=cubeLimits$tlim,xlab="",ylab="",zlab="Frame Number",aspect=T)
  bg3d("black") 
}



#' plot2D.TrajectoryList
#' 
#' Provides a bird's eye view of a TrajectoryList object on a bidimensional space
#' 
#' Independent from the rgl library, the function extracts the region of interests from the dimensions of an image of the FrameList object,
#' and afterwards plots the x-y-time representation of the identified trajectories on a 2d plane. It is possible to subset the TrajectoryList
#' object with the IDs of the desired trajectories
#' 
#' @param x A TrajectoryList object
#' @param framelist A FrameList object, used here to identify the limits of the region of interest 
#' @param trajIDs A vector containing the ids of the desired trajectories
#' @param ... Arguments to be passed to methods

#' @examples
#' load(file.path(system.file("extra", package="flowcatchR"),"MesenteriumSubset.RData"))
#' load(file.path(system.file("extra", package="flowcatchR"),"candidate.platelets.RData"))
#' platelets.trajectories <- trajectories(candidate.platelets)
#' plot2D.TrajectoryList(platelets.trajectories,MesenteriumSubset)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
plot2D.TrajectoryList <- function(x,framelist,trajIDs=NULL,...)
{
  cubeLimits <- axesInfo(framelist)
  xlim <- cubeLimits$xlim
  ylim <- cubeLimits$ylim
  
  colcols <- rep(colorRamps::primary.colors(40,steps=10,F),6)
  if(is.null(trajIDs))
  {
    trajIDs <- 1:length(x)
  } else {
    # check the IDs are correctly given
    allAvailableTrajectories <- sapply(x,function(arg){unique(arg$trajectory$trajLabel)})
    if(!all(trajIDs %in% allAvailableTrajectories))
      stop("You are supplying IDs of trajectories which are not included in the TrajectoryList object!")
  }
  cat("Plotting",length(trajIDs),"trajectories (total available in the TrajectoryList:", length(x), ")...\n")
  
#   trajectoryDataFrame <- do.call(rbind.data.frame,lapply(x,function(arg){arg$trajectory}))
  t <- trajIDs[1]
  plot(yCoord~xCoord, data = x[[t]]$trajectory,
     xlim=xlim, ylim=ylim, xlab = "Pixel Coordinates - x axis",ylab="Pixel Coordinates - y axis",
     col=colcols[t],type = "l", lty = 1,lwd = 3,
     main= "Overview of the identified trajectories")
#   text((yCoord + 2)~xCoord, data = x[[t]]$trajectory, labels=t,
#      col=colcols[t])
  # plotting just the label for the first point
  text((yCoord[1] + 4)~xCoord[1], data = x[[t]]$trajectory, labels=t,
     col=colcols[t])
  for (t in trajIDs[-1])
  {
    lines(yCoord~xCoord, data = x[[t]]$trajectory,
          col=colcols[t],type = "l", lty = 1,lwd = 3)
    text((yCoord[1] + 4)~xCoord[1], data = x[[t]]$trajectory, labels=t,
         col=colcols[t])
  }
#   cubeLimits <- axesInfo(framelist)
#   decorate3d(xlim=cubeLimits$xlim,ylim=cubeLimits$ylim,zlim=cubeLimits$tlim,xlab="",ylab="",zlab="",aspect=T)
  
}




#' add.contours
#' 
#' Creates a FrameList objects containing raw information, combined with the segmented images and the relative trajectory under analysis
#' 
#' If a TrajectoryList is provided and mode is set to "trajectories", returns a FrameList with all trajectories included in the IDs 
#' vector painted accordingly.
#' If the mode is set to "particles", it will just plot the particles (all) on all frames.
#' If no trajectoryList is provided, it will be computed with default parameters.
#' If no binary.frames is provided, it will be computed also with default parameters
#' 
#' @param raw.frames A FrameList object with raw images
#' @param binary.frames A FrameList object with preprocessed frames
#' @param trajectories A trajectories object
#' @param trajIds Numeric vector, the ID(s) of the trajectory.
#' @param mode A character string, can assume the values "particles" or "trajectories". Defaults to "particles"
#' @param col A vector of color strings
#' @param channel A character string, to select which channel to process, if a ChannelsFrameList is supplied or if the FrameList in raw.frames has more than one channel
#' 
#' @return A new FrameList object
#' 
#' @examples
#' load(file.path(system.file("extra", package="flowcatchR"),"MesenteriumSubset.RData"))
#' \dontrun{
#' paintedTrajectories <- add.contours2(raw.frames = MesenteriumSubset, mode = "trajectories",channel="red")
#' paintedParticles <- add.contours2(raw.frames = MesenteriumSubset, mode = "particles",channel="red")
#' inspect.frames(paintedTrajectories)
#' inspect.frames(paintedParticles)
#' }
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
add.contours <- function(raw.frames,
                          binary.frames=NULL,
                          trajectories=NULL,
                          trajIds=NULL,
                          mode="particles", # could assume also "trajectories" as a value
                          col=NULL, 
                          channel=NULL)
{
  # store for combining steps
  input.frames <- raw.frames # and leave untouched
  # check whether the input has more than one channel in the FrameList
  if(is(raw.frames,"FrameList"))
  {
    nrChannels <- getNumberOfFrames(raw.frames[[1]]$image)
    if(nrChannels > 1)
    {
      raw.frames <- channels(raw.frames)
    } #else, it has already just one channel and we can proceed
  }
  
  # if no preprocessed is provided...
  if(is.null(binary.frames))
  {
    # compute them with the defaults
    if(is(raw.frames,"ChannelsFrameList"))
    {
      if(is.null(channel))
      {
        stop("You need to provide a channel along with a ChannelsFrameList - or maybe you supplied a FrameList object with more than one channel to the raw.frames parameter?")
      }
#     framelistRaw <- raw.frames[[channel]]
      binary.frames <- preprocess.ChannelsFrameList(raw.frames,channel)
    } else {
      binary.frames <- preprocess.FrameList(raw.frames)
    }
    # check also whether trajectories are there already, if so throw an error # or a warning?
    if(!is.null(trajectories))
    {
      stop("You are providing a TrajectoryList object, but no binary.frames FrameList! Please check whether you might have it in your workspace!")
    }
  }

  # 
  if(mode=="trajectories") # additional checks on the trajectorylist object provided 
  {
    # if no trajectorylist is provided, compute it
    if(is.null(trajectories))
    {
      trajectories <- trajectories(particles(input.frames,binary.frames))
    }
    
    availableIDs <- unlist(lapply(trajectories,function(arg){arg$ID}))
    # if no single ids are provided, do it for all
    if(is.null(trajIds))
    {
      # will do it on all, so update it to all the available ones
      trajIds <- availableIDs
   
    } else {
      # will do it by looping on the length of the provided trajectory IDs vector
      # check that the trajectories are indeed "plottable"!
      if(!all(trajIds %in% availableIDs))
      {
        stop("You are providing IDs of trajectories that are not available. Please run unlist(lapply(trajectories,function(arg){arg$ID})) on the trajectory object!")
      }
    }
    
    # actually does it for both cases above
#     library(colorRamps)
    colcols <- rep(colorRamps::primary.colors(40,steps=10,F),6)
    out <- input.frames
    for(i in trajIds)
    {
      # same as above actually -> make it a function? # compacted like this
#       cat("Doing",i,"-\n")
#       browser()
      currentTraj <- trajectories[[i]]$trajectory
      counter <- 1
      for(j in currentTraj$frame)
      {
        rawimg <- out[[j]]$image
        segmimg <- binary.frames[[j]]$image
        singleObjectSegm <- segmimg
        singleObjectSegm[segmimg!=currentTraj$frameobjectID[counter]] <- 0
#         cat("max obj",max(segmimg),"---",length(as.data.frame(table(singleObjectSegm))$singleObjectSegm)-1,"\t\t")
        
        rawWithPaintedObj <- paintObjects(singleObjectSegm,rawimg,col=colcols[i])
#         cat(paste0("traj",i, "\tframe",j,"\t",colcols[i]))
        out[[j]]$image <- rawWithPaintedObj 
#         cat("\n")
        counter <- counter +1
      }
    }  

    
  } else if(mode=="particles") # there is actually no need for the trajectorylist/trajId, and the col can be just one value ;)
  {
    out <- combine.preprocessedFrameList(input.frames,binary.frames,col)
   
  } else {
    stop("The mode parameter must assume one value of the following: 'particles' or 'trajectories'!")
  }
    


  return(out)
}














## replaced basically by explicit text in the vignette
# #' inspect.trajectory
# #' 
# #' A function to inspect single trajectories from a TrajectoryList object
# #' 
# #' Basically it add the contours to just one object, and returns a FrameList object which is a subset of the input one
# #' The length of the returned object corresponds exactly the length of the identified trajectory, so uninteresting frames will be skipped
# #' 
# #' 
# #' OLD: A pseudo-interactive function to inspect and accept/reject trajectories of a TrajectoryList object
# #' OLD: The user gets the trajectories displayed singularly, and for each a keyboard input is required.
# #' The slot corresponding to the user feedback is interactively filled in. The TrajectoryList object is then ready for further operations
# #' 
# #' @param trajectorylist A TrajectoryList object
# #' @param rawframelist A FrameList object with raw images
# #' @param preprocessedframelist A FrameList object with preprocessed frames
# #' @param trajID A numeric value, storing the ID of trajectory which is under inspection
# #' @param browse Logical, whether to automatically open the painted trajectory to inspect
# #' 
# #' @return A new TrajectoryList object -NO! Returns now a FrameList object, and this can be used to evaluate the trajectory
# #'
# #' @examples
# #' ## see vignette
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# inspect.trajectory <- function(trajectorylist,
#                                  rawframelist,
#                                  preprocessedframelist,
#                                  trajID,
#                                  browse=TRUE)
# {
#   currentTraj <- trajectories[[trajID]]$trajectory
#   framesIncluded <- currentTraj$frame
#   subsetRaw <- subset(rawframelist,framesToKeep=framesIncluded)
#   subsetProcessed <- subset(preprocessedframelist,framesIncluded)
#   
#   out <- vector("list",length(framesIncluded))
#   
#   for (j in 1:length(out))
#   {
#     rawimg <- subsetRaw[[j]]$image
#     segmimg <- subsetProcessed[[j]]$image
#     singleObjectSegm <- segmimg
#     singleObjectSegm[segmimg!=currentTraj$frameobjectID[j]] <- 0
#     rawWithPaintedObj <- paintObjects(singleObjectSegm,rawimg,col="yellow") #  even more beautiful if i have red cells displayed and contour as YELLOW!
#     
#     out[[j]]$image <- rawWithPaintedObj 
#   }
#   class(out) <- c("FrameList",class(out))
#   if(browse) inspect.frames(out)
#   return(out)
# }

  
  
  




