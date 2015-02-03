
#' Add object contours to a \code{Frames} object
#'  
#' Creates a \code{Frames} object containing raw information, combined with the segmented images and the relative trajectory under analysis
#' 
#' If a \code{TrajectorySet} is provided and mode is set to \code{trajectories}, returns a \code{Frames} with all trajectories included in the IDs 
#' vector painted accordingly.
#' If the mode is set to \code{particles}, it will just plot the particles (all) on all frames.
#' If no \code{TrajectorySet} is provided, it will be computed with default parameters.
#' If no \code{binary.frames} is provided, it will be computed also with default parameters
#' 
#' @param raw.frames A \code{Frames} object with raw images
#' @param binary.frames A \code{Frames} object with preprocessed frames
#' @param trajectoryset A \code{TrajectorySet} object
#' @param trajIDs Numeric vector, the ID(s) of the trajectory.
#' @param mode A character string, can assume the values \code{particles} or \code{trajectories}. Defaults to \code{particles}
#' @param col A vector of color strings
#' @param channel A character string, to select which channel to process
#' 
#' @return A new \code{Frames} object with contours of the objects added
#' 
#' @examples
#' data("MesenteriumSubset")
#' \dontrun{
#' paintedTrajectories <- add.contours(raw.frames = MesenteriumSubset, mode = "trajectories",channel="red")
#' paintedParticles <- add.contours(raw.frames = MesenteriumSubset, mode = "particles",channel="red")
#' inspect.Frames(paintedTrajectories)
#' inspect.Frames(paintedParticles)
#' }
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
add.contours <- function(raw.frames,
                         binary.frames=NULL,
                         trajectoryset=NULL,
                         trajIDs=NULL,
                         mode="particles", # could assume also "trajectories" as a value
                         col=NULL, 
                         channel=NULL)
{
  # store for combining steps
  #   input.frames <- raw.frames # and leave untouched
  if(!is.null(binary.frames))
    channel <- binary.frames@channel
  if(raw.frames@channel == "all" && is.null(channel))
    stop("Please provide a channel to work on for preprocessing and tracking")
  
  # if no preprocessed is provided
  if(is.null(binary.frames))
  {
    binary.frames <- preprocess.Frames(channel.Frames(raw.frames,channel))
    # check also whether trajectories are there already, if so throw an error # or a warning?
    if(!is.null(trajectoryset))
    {
      stop("You are providing a TrajectorySet object, but no binary.frames Frames object!
           Please check whether you might have it in your workspace")
    }
    }
  
  
  # 
  if(mode=="trajectories") # additional checks on the trajectoryset object provided 
  {
    # if no trajectoryset is provided, compute it
    if(is.null(trajectoryset))
    {
      trajectoryset <- trajectories(particles(raw.frames,binary.frames,channel=channel))
    }
    
    availableIDs <- unlist(lapply(trajectoryset,function(arg){arg$ID}))
    # if no single ids are provided, do it for all
    if(is.null(trajIDs))
    {
      # will do it on all, so update it to all the available ones
      trajIDs <- availableIDs
      
    } else {
      # will do it by looping on the length of the provided trajectory IDs vector
      # check that the trajectories are indeed "plottable"!
      if(!all(trajIDs %in% availableIDs))
      {
        stop("You are providing IDs of trajectories that are not available. 
             Please run unlist(lapply(trajectoryset,function(arg){arg$ID})) on the trajectory object!")
      }
      }
    
    out <- addTrajectories(raw.frames,binary.frames,trajectoryset,trajIDs,col)
    
    } else if(mode=="particles") # there is actually no need for the trajectoryset/trajId, and the col can be just one value ;)
    {
      out <- addParticles(raw.frames,binary.frames,col)
      
    } else {
      stop("The mode parameter must assume one value of the following: 'particles' or 'trajectories'!")
    }
  
  return(out)
  }





# addTrajectories <- function(raw.frames,binary.frames,trajectoryset,trajIDs,col){
#   nrFrames <- length.Frames(raw.frames)
#   if(colorMode(raw.frames)==0)
#     raw.frames <- Frames(rgbImage(red=raw.frames),channel = "all")
#   
#   tmpFL <- lapply(1:nrFrames,function(i) Image(getFrame(raw.frames,i,"render")))
#   
#   if(is.null(col)) {
#     colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
#     # force to yellow if just one
#     if(length(trajIDs)==1) 
#       colcols <- "yellow"
#   } else {
#     colcols <- rep(col,240)
#   }
#   
#   for(i in trajIDs)
#   {
#     currentTraj <- trajectoryset[[i]]$trajectory
#     counter <- 1
#     for(j in currentTraj$frame)
#     {
#       rawimg <- tmpFL[[j]]
# #       # if black and white, helps enhancing detection/tracking results
# #       if(colorMode(rawimg)==0)
# #         rawimg <- rgbImage(red=rawimg)
#       segmimg <- Image(getFrame(binary.frames,j,"render"))
#       singleObjectSegm <- segmimg
#       singleObjectSegm[segmimg!=currentTraj$frameobjectID[counter]] <- 0
#       
#       rawWithPaintedObj <- paintObjects(singleObjectSegm,rawimg,col=colcols[i])
#       
#       tmpFL[[j]] <- rawWithPaintedObj 
#       counter <- counter +1
#     }
#   }  
#   out <- Frames(combine(tmpFL),channel="all")
#   return(out)
# }


addTrajectories <- function(raw.frames,binary.frames,trajectoryset,trajIDs,col){
  nrFrames <- length(raw.frames)
  if(colorMode(raw.frames)==0)
    raw.frames <- Frames(rgbImage(red=raw.frames),channel = "all")
  
  tmpFL <- lapply(1:nrFrames,function(i) Image(getFrame(raw.frames,i,"render")))
  
  if(is.null(col)) {
    colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
    # force to yellow if just one
    if(length(trajIDs)==1) 
      colcols <- "yellow"
  } else {
    colcols <- rep(col,240)
  }
  
  for (i in 1:nrFrames)
  {
    rawimg <- tmpFL[[i]]
    counter <- 1
    segmimg <- Image(getFrame(binary.frames,i,"render"))
    
    for(j in trajIDs)
    {
      # only if the trajectory j is involved in the frame i, do the real painting
      if(i %in% trajectoryset[[j]]$trajectory$frame) {
        singleObjectSegm <- segmimg # reset
        # put to zero everything not the object under analysis
        currTraj <- trajectoryset[[j]]$trajectory
        singleObjectSegm[segmimg!=currTraj$frameobjectID[which(currTraj$frame==i)]] <- 0
        # paint
        rawWithPaintedObj <- paintObjects(singleObjectSegm,tmpFL[[i]],col=colcols[j])
        # update the img for the next round of the loop
        tmpFL[[i]] <- rawWithPaintedObj
      } # else { # # if the trajectory j is not involved in the frame i, do nothing
        #
    #  }      
    }
  }

  out <- Frames(combine(tmpFL),channel="all")
  return(out)
}








#' Combines the information from a raw \code{Frames} object and the corresponding preprocessed one
#' 
#' All objects are painted with a unique colour - for sake of speed
#'  
#' @param raw.frames A \code{Frames} object containing the raw images
#' @param binary.frames A \code{Frames} object with the preprocessed versions of the images (e.g. segmented)
#' @param col A color character string, to select which color will be used for drawing the contours of the particles. If not specified, it will default according to the objects provided
#' 
#' @return A \code{Frames} object, whose images are the combination of the raw images with the segmented objects drawn on them
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
addParticles <- function(raw.frames,binary.frames,col=NULL) {
  if(is.null(col)) col <- "yellow"
  # if raw images are in black and white, use false colors to enhance the marking of detected cells
  if(colorMode(raw.frames)==0)
  {
    out <- paintObjects(binary.frames, rgbImage(red = raw.frames), col=col)
  } else {
    out <- paintObjects(binary.frames, raw.frames, col=col)
  }
  return(out)
}




#' Match trajectories to related particles.
#' 
#' Match trajectories to the related particles in the \code{TrajectorySet} and 
#' \code{ParticleSet} objects. This function returns a new \code{ParticleSet}
#' object that contains as additional column the trajectory ID that the particular
#' particle was assigned to. Used also by other routines, such as \code{\link{snap}}
#' 
#' @param particleset A \code{ParticleSet} object
#' @param trajectoryset A \code{TrajectorySet} object coupled to the \code{particleset}
#' 
#' @return A \code{ParticleSet} object with an additional column with the trajectory
#' IDs 
#' 
#' @examples
#' data(candidate.platelets)
#' trajs <- trajectories(candidate.platelets)
#' matchTrajToParticles(candidate.platelets, trajs)
#' 
#' @export
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2015
matchTrajToParticles <- function(particleset,trajectoryset)
{
  myTrajParts <- particleset
  for(i in 1:length(myTrajParts)) {
    tmpdf <- myTrajParts[[i]]
    tmpdf$correspTrajId <- rep(NA,nrow(tmpdf))
    myTrajParts[[i]] <- tmpdf
  }
  
  for (j in 1:length(trajectoryset))
  {
    #     cat("\nnewtraj---",j)
    framesInvolved <- trajectoryset[[j]]$trajectory$frame
    for(k in 1:trajectoryset[[j]]$npoints)
    {
      #       cat("point",k)
      partIDinTraj <- trajectoryset[[j]]$trajectory$frameobjectID[k]
      myTrajParts[[ framesInvolved[k] ]]$correspTrajId[partIDinTraj] <- j
    }
  }
  return(myTrajParts)
}




#' Snap the features of the closest particle identified
#' 
#' This function combines all classes related to a single experiment in order to deliver
#' a clickable feedback on one of the frames.
#' 
#' @param raw.frames A \code{Frames} object with the raw frames data
#' @param binary.frames A \code{Frames} object with the preprocessed frames data
#' @param particleset A \code{ParticleSet} object with the particles data
#' @param trajectoryset A \code{TrajectorySet} object with the trajectories data
#' @param frameID The ID of the frame to inspect 
#' @param infocol The color to use for plotting the contours and the information on the 
#' clicked particle
#' @param infocex The numeric character expansion value as in \code{cex} to be used
#' for printing the text on the image
#' @param showVelocity Logical, whether to display additional information on the 
#' instantaneous velocity of the particle
#' 
#' @return An image of the selected frame, rendered in R native graphics, and additionally 
#' a list with the coordinates as well as the trajectory ID of the particle closest to the
#' clicked location
#' 
#' @examples
#' \dontrun{data(MesenteriumSubset)
#' binary.frames <- preprocess.Frames(channel.Frames(MesenteriumSubset,"red"))
#' particleset <- particles(MesenteriumSubset,binary.frames,"red")
#' trajectoryset <- trajectories(particleset)
#' snap(MesenteriumSubset,binary.frames,particleset,trajectoryset,frameID=1)
#' }
#' @export
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2015
snap <- function(raw.frames,
                 binary.frames,
                 particleset,
                 trajectoryset,
                 frameID = 1,
                 infocol = "yellow",
                 infocex = 1,
                 showVelocity = FALSE)
{
  imageToPaintOn <- getFrame(raw.frames,frameID,type = "render")
  correspondingBinary <- getFrame(binary.frames,frameID,type = "render")
  # TODO: maybe display already somehow additional info with the borders of the detected ones? - e.g. in some dark orange, so that 
  # the yellow one still pops up?
  display(imageToPaintOn,method="raster")
  coordsClick <- locator(1)
  coordsClick
  particlesLocations <- particleset[[frameID]][,1:2] # on the frame corresponding to the selected one
  # compute euclidean distance to each from the location of the mouse click
  distances <- data.frame(particles=row.names(particleset[[frameID]]),
                          distToThis=rep(0,nrow(particleset[[frameID]]))
                         )
  myX <- coordsClick$x
  myY <- coordsClick$y
  
  # to decide where to place the info label
  imgSizeX <- dim(imageToPaintOn)[1]
#   imgSizeY <- dim(imageToPaintOn)[2]
  leftSide <- (myX < imgSizeX/2)
#   upSide <- (myY < imgSizeY/2)
  
  distToClick <- unlist(lapply(1:nrow(particlesLocations),
                               function(arg){
                                 myDist <- sqrt( (myX - particlesLocations$cell.0.m.cx[arg])^2 + (myY - particlesLocations$cell.0.m.cy[arg])^2 )
                               }))
  distances$distToThis <- distToClick
  
  partToDraw <- which.min(distances$distToThis)
  
  # put to zero everything but
  binaryObj <- correspondingBinary
  binaryObj[correspondingBinary!=partToDraw] <- 0
  imgWithParticlePainted <- paintObjects(binaryObj,imageToPaintOn,col=infocol)
  display(imgWithParticlePainted,method="raster")
  
  matchedParticles <- matchTrajToParticles(particleset,trajectoryset)
  
  trajCorrespID <- matchedParticles[[frameID]]$correspTrajId[[partToDraw]]
  particleInfo <- list(x = particleset[[frameID]]$cell.0.m.cx[[partToDraw]],
                       y = particleset[[frameID]]$cell.0.m.cy[[partToDraw]],
                       trajectoryID = trajCorrespID)
  
  if(showVelocity){
    allKinematics <- kinematics(trajectoryset)
    if(frameID != trajectoryset[[trajCorrespID]]$npoints) # must be less than the last point of the traj
      instantVel <- allKinematics[[trajCorrespID]][["delta.v"]][[frameID]]
  }
  
  textToDisplay <- paste0(round(particleInfo$x,3),";",round(particleInfo$y,3)," - traj ",particleInfo$trajectoryID)
  if(showVelocity)
    textToDisplay <- paste0(textToDisplay," - vel_i ",round(instantVel,3))
  
  
  if(leftSide)
  {
    whereToPutTheText <- 4
  } else {
    whereToPutTheText <- 2
  }
  
  
  text(labels = textToDisplay,
       x = (particleInfo$x + 2 * ifelse(leftSide,1,-1)),
       y = (particleInfo$y + 2 ),
       col = infocol,
       cex = infocex,
       pos = whereToPutTheText
       )
  
  return(particleInfo)
}




