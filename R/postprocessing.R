
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





addTrajectories <- function(raw.frames,binary.frames,trajectoryset,trajIDs,col){
  nrFrames <- length.Frames(raw.frames)
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
  
  for(i in trajIDs)
  {
    currentTraj <- trajectoryset[[i]]$trajectory
    counter <- 1
    for(j in currentTraj$frame)
    {
      rawimg <- tmpFL[[j]]
#       # if black and white, helps enhancing detection/tracking results
#       if(colorMode(rawimg)==0)
#         rawimg <- rgbImage(red=rawimg)
      segmimg <- Image(getFrame(binary.frames,j,"render"))
      singleObjectSegm <- segmimg
      singleObjectSegm[segmimg!=currentTraj$frameobjectID[counter]] <- 0
      
      rawWithPaintedObj <- paintObjects(singleObjectSegm,rawimg,col=colcols[i])
      
      tmpFL[[j]] <- rawWithPaintedObj 
      counter <- counter +1
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








