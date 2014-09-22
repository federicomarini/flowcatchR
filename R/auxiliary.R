#' read.frames
#' 
#' constructor for a FrameList object
#' 
#' This function is used to create a FrameList object from a vector of images. 
#' The number of frames is also specified, as just a subset of the images can be used for this
#' 
#' @param image.files Vector of strings containing the locations where the (raw) images are to be found, or alternatively, the path to the folder
#' @param nframes Number of frames that will constitute the FrameList object
#'
#' @return An object of the \code{FrameList} class, which holds the info on a list of frames, specifying for each the following elements:
#' \item{image}{The \code{Image} object containing the image itself}
#' \item{location}{The complete path to the location of the original image}
#' 
#'
#' @examples
#' ## see vignette
#' \dontrun{fullData <- read.frames(image.files = "/path/to/the/directory", nframes = 100)}
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
read.frames <- function(image.files,
                        nframes=NULL)
{
  cat("Creating a new object of class FrameList...\n")
  is.dir <- file.info(image.files[1])$isdir
  if(is.dir){
    image.files <- image.files[1]
    cat("Reading images from directory", image.files,"...\n")
    image.files <- list.files(image.files, pattern='*.jpg$|*.jpeg$|*.tiff$|*.tif$|*.png$', full.names=TRUE, ignore.case=TRUE)
    if(length(image.files) == 0) 
      stop('No images with jpeg/jpg, png or tiff/tif extension found. Images must be available in one of these formats,
           please convert them prior to importing into flowcatchR')
  }
  
  z <- sapply(image.files, file.exists)
  if(!all(z)) 
    stop(sprintf('Files "%s" do not exist', paste0(image.files[!z], collapse=', ')))
  
  if(is.null(nframes))
    nframes <- length(image.files)
  
  # check that nframes coincides with the number of images available -throw an error otherwise?
  frameList <- vector(nframes,mode="list")
  class(frameList) <- c("FrameList",class(frameList))

  for (i in 1:nframes)
  {
    frameList[[i]]$image <- readImage(image.files[i])
    frameList[[i]]$location <- image.files[i]
    # or should i just use     frameList[[i]] <- readImage(imgsLocation[[i]])

  }
  attr(frameList,"call") <- match.call()
  cat("Created a frameList object of",nframes,"frames.\n")
  return(frameList)
}

#' print.FrameList
#' 
#' Method for displaying conveniently a FrameList object
#' 
#' @param x A FrameList object
#' @param ... Arguments to be passed to methods
#'  
#' @method print FrameList
#' 
#' @examples
#' data("MesenteriumSubset")
#' print(MesenteriumSubset)
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
print.FrameList <- function(x,...)
{
  d <- dim(x[[1]]$image)
  cat("An object of the FrameList class. \n\n")
  cat("List of frames for",length(x),"images\n")
  cat("Images contain information on",ifelse(!is.na(d[3]),d[3],"1"),"channel(s)\n")
  cat("Image dimensions:\t",d[1:2],"\n")
}




#' inspect.frames
#' 
#' Explore the first frames of a FrameList
#' 
#' The first frames of a FrameList are displayed in the browser, and are interactively navigable.
#' Default number of shown frames is 4, can be set maximum to 8, as this function is purely for a 
#' first inspection
#' 
#' @param framelist A FrameList object
#' @param nframes The number of frames to display (default value: 6)
#' @param inspectAll Logical, whether to inspect all frames (overriding the default of 10 that can be used also when inspectAll is FALSE)
#' @param display.method Method for displaying, can be either "raster" or "browser". Defaults to browser, by opening a window in the browser
#' @param verbose Logical, whether to provide additional output on the command line alongside with the images themselves
#' 
#' @examples
#' data("MesenteriumSubset")
#' \dontrun{inspect.frames(MesenteriumSubset)}
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
inspect.frames <- function(framelist,
                           nframes=NULL,
                           inspectAll=FALSE,
                           display.method="browser",
                           verbose=FALSE)
{
  if(is.null(nframes))
  {
    if(inspectAll)
    {
      nframes <- length(framelist)
    } else {
      nframes <- min(10,length(framelist)) # set to 10 as default, or to the length of the object itself, bzw which is shorter
    }
  } 
  
  if(nframes > 10)
  {
    if(!inspectAll)
    {
      stop("Too high number of frames, try with a number <= 10")
    }
    cat("You selected to inspect a number of frames > 10, this might take a while...\n")
    
  }
  # nframes should also be more than the nr of frames available
  if (verbose) cat("Displaying the first",nframes,"frames for the FrameList")
  
  firstFrames <- list()
  
  # need to set the colorMode to Grayscale if it is only one channel so that combine works!!
  
  singleChannel <- !(length(dim(framelist[[1]]$image)) > 2)
  
  firstFrames[[1]] <- framelist[[1]]$image
#   if(singleChannel) 
#   {
#     colorMode(firstFrames[[1]]) <- Grayscale
#   }
  for (i in 2:nframes)
  {
    firstFrames[[i]] <- framelist[[i]]$image
#     if(singleChannel)
#     {
#       colorMode(firstFrames[[i]]) <- Grayscale
#     }
  }
  
  firstFramesCombined <- combine(firstFrames)
  display(firstFramesCombined,all=TRUE,method=display.method)
}







#' subset.FrameList
#' 
#' Extracts a subset of frames from a FrameList object
#' 
#' An input FrameList object is subject to subsetting. This function is useful e.g. when the trajectory of interest 
#' is presenting gaps (i.e. does not actually include a frame)
#' 
#' @param x A FrameList object
#' @param framesToKeep A vector containing the indexes of the frames to keep in the selection
#' @param ... Arguments to be passed to methods
#' 
#' @return A FrameList object, composed by the subset of frames of the input FrameList
#' 
#' @method subset FrameList
#' 
#' @examples
#' data("MesenteriumSubset")
#' subset(MesenteriumSubset, framesToKeep = c(1:10, 14:20))
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
subset.FrameList <- function(x,framesToKeep,...)
{
  # check that the frames to keep are actually in the original framelist?
  out <- vector("list",length(framesToKeep))
  counter <- 0
  for (i in 1:length(x))
  {
    if(i %in% framesToKeep)
    {
      counter <- counter + 1
      out[[counter]] <- x[[i]]
    }
  }
  class(out) <- c("FrameList",class(out))
  return(out)
}


#' channels
#' 
#' Creates a ChannelsFrameList object from a FrameList object, decomposed in the acquired channels
#'  
#' @param framelist A FrameList object
#' 
#' @return A ChannelsFrameList object, which is a list of 3 FrameList objects, named respectively red, green and blue
#' 
#' @export
#' 
#' @examples 
#' data("MesenteriumSubset")
#' channels(MesenteriumSubset)
#' plateletsFrameList <- channels(MesenteriumSubset)$red
#' plateletsFrameList
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
channels <- function(framelist)
{
  # check if the framelist indeed has more than one channel, first of all! -> needs to be done TODO
  redCh <- framelist
  greenCh <- framelist
  blueCh <- framelist
  
  for (i in 1:length(framelist))
  {
    redCh[[i]]$image <- channel(framelist[[i]]$image,"red")
    redCh[[i]]$channel="red"
    greenCh[[i]]$image <- channel(framelist[[i]]$image,"green")
    greenCh[[i]]$channel <- "green"
    blueCh[[i]]$image <- channel(framelist[[i]]$image,"blue")
    blueCh[[i]]$channel <- "blue"
  }
  
  channelsframelist <- list(red=redCh,green=greenCh,blue=blueCh)
  class(channelsframelist) <- c("ChannelsFrameList",class(channelsframelist))
  return(channelsframelist)
}




#' export.frames
#' 
#' Exports a FrameList object
#' 
#' Writes the images contained in the image slot of the FrameList object elements.
#' The images can be exported as single frames, or as a .gif image that is composed
#' by the single frames.
#' 
#' @param framelist A FrameList object
#' @param dir The path of the folder where the image should be written
#' @param nameStub The stub for the file name, that will be used as a prefix for the exported images
#' @param createGif Logical, whether to create or not an animated .gif file
#' @param removeAfterCreatingGif Logical, whether to remove the single exported .png images after creating the single .gif
#' 
#' @return Image files are written in the desired location
#' 
#' @examples
#' data("MesenteriumSubset")
#' \dontrun{export.frames(MesenteriumSubset,nameStub="subset_export_",createGif=TRUE,removeAfterCreatingGif=FALSE)}
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
export.frames <- function(framelist,
                          dir=tempdir(),
                          nameStub="testExport",
                          createGif=FALSE,
                          removeAfterCreatingGif=TRUE)
{
  if(!file.exists(dir))
  {
    dir.create(dir,showWarnings=FALSE) # if not already existing...
  }
  imgNames <- lapply(1:length(framelist),
                     function(arg){paste0(dir,nameStub,"_frame_",formatC(arg,nchar(length(framelist)),flag="0"),".png")})
  for (i in 1:length(framelist))
  {
    writeImage(framelist[[i]]$image,imgNames[[i]])
  }
  if(createGif)
  {
    # using imagemagick
    system(paste0("convert -delay 40 ",dir,nameStub,"_frame_*.png ",dir,nameStub,".gif"))
  }
  if(removeAfterCreatingGif)
  {
    file.remove(list.files(path=dir,pattern=paste0(".png"),full.names=TRUE))
  }
  invisible()
}





#' export.particles
#' 
#' Exports a ParticleList object
#' 
#' Writes the particles contained in the particles data frame slot of the ParticleList object elements.
#' A track of the provenience of the particles is stored as a comment line above the header
#' 
#' @param particlelist A ParticleList object
#' @param dir The path of the folder where the particle lists should be written
#' @param nameStub The stub for the file name, that will be used as a prefix for the exported particle lists
#' 
#' @return Particle list files are written in the desired location
#' 
#' @examples
#' data("candidate.platelets")
#' \dontrun{export.particles(candidate.platelets)}
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
export.particles <- function(particlelist,
                          dir=tempdir(),
                          nameStub="testExport_particles_")
{
  cat("Exporting the .tsv files for the particle lists...\n")
  if(!file.exists(dir))
  {
    dir.create(dir,showWarnings=FALSE) # if not already existing...
  }
  particleNames <- lapply(1:length(particlelist),
                          function(arg){paste0(dir,nameStub,"_frame_",formatC(arg,nchar(length(particlelist)),flag="0"),".tsv")})
  for (i in 1:length(particlelist))
  {
    writeLines(paste("#",particlelist[[i]]$imgSource,particlelist[[i]]$channel,sep = "|"),particleNames[[i]])
    write.table(particlelist[[i]]$particles,particleNames[[i]],append = TRUE,sep = "\t",quote = FALSE,col.names = TRUE,row.names= FALSE)
  }
  cat("Done exporting the .tsv files for the particle lists.\n
      You can ignore the warning messages as long as you remind the additional comment line added starting with '#'")
  invisible()
}


#' read.particles
#' 
#' constructor for a ParticleList object
#' 
#' This function is used to create a ParticleList object from a vector/list of tab separated text files, each of one containing one line for each 
#' particle in the related frame, alongside with its coordinates and if available, the computed features
#' The number of frames is also specified, as just a subset of the particle lists can be used for this
#' 
#' @param particle.files Vector of strings containing the locations where the particle coordinates are to be found, or alternatively, the path to the folder
#' @param nframes Number of frames that will constitute the ParticleList object
#'
#' @return An object of the \code{ParticleList} class 
#' 
#' @examples
#' ## see vignette and export.particles
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
read.particles <- function(particle.files,
                           nframes=NULL)
{
  cat("Creating a new object of class ParticleList...\n")
  is.dir <- file.info(particle.files[1])$isdir
  if(is.dir){
    particle.files <- particle.files[1]
    cat("Reading particles from directory", particle.files,"...\n")
    particle.files <- list.files(particle.files, pattern='*.tsv$', full.names=TRUE, ignore.case=TRUE)
    if(length(particle.files) == 0) 
      stop('No particle files with .tsv extension found. Particles must be provided in .tsv format, please check whether the extension is .txt and change it ')
  }
  
  z <- sapply(particle.files, file.exists)
  if(!all(z)) 
    stop(sprintf('Files "%s" do not exist', paste0(particle.files[!z], collapse=', ')))
  
  if(is.null(nframes))
    nframes <- length(particle.files)
  
  # check that nframes coincides with the number of images available -throw an error otherwise?
  particleList <- vector(nframes,mode="list")
  class(particleList) <- c("ParticleList",class(particleList))
  
  for (i in 1:nframes)
  {
    particleList[[i]]$particles <- read.table(particle.files[i],sep="\t",comment.char = "#",header=TRUE,stringsAsFactors = FALSE)
    commentLine <- readLines(particle.files[i],n=1)
    particleList[[i]]$imgSource <- unlist(strsplit(commentLine,split = "|",fixed=TRUE))[2]
    particleList[[i]]$channel <- unlist(strsplit(commentLine,split = "|",fixed=TRUE))[3]
  }
#   attr(particleList,"call") <- match.call()
  cat("Created a particleList object of",nframes,"frames.\n")
  return(particleList)
}


#' print.ParticleList
#' 
#' Method for displaying conveniently a ParticleList object
#'  
#' @param x A ParticleList object
#' @param ... Arguments to be passed to methods
#' 
#' @method print ParticleList
#' 
#' @examples
#' data("candidate.platelets")
#' print(candidate.platelets)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
print.ParticleList <- function(x,...)
{
  firstFrameParticles <- x[[1]]$particles
  cat("An object of the ParticleList class. \n\n")
  cat("List of particles for",length(x),"images\n\n")
  cat("Displaying a subset of the features of the",nrow(firstFrameParticles),"particles found in the first image...\n")
  linesToShow <- min(5,nrow(firstFrameParticles))
  print(firstFrameParticles[1:linesToShow,1:8])
  cat("\nParticles identified on the",x[[1]]$channel,"channel\n")
}

#' print.LinkedParticleList
#' 
#' Method for displaying conveniently a LinkedParticleList object
#'  
#' @param x A LinkedParticleList object
#' @param ... Arguments to be passed to methods
#' 
#' @method print LinkedParticleList
#' 
#' @examples
#' data("candidate.platelets")
#' linked.platelets <- link.particles(candidate.platelets,L=26,R=3,epsilon1=0,
#' epsilon2=0,lambda1=1,lambda2=0,penaltyFunction=penaltyFunctionGenerator(),
#' nframes=20,include.area=FALSE)
#' print(linked.platelets)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
print.LinkedParticleList <- function(x,...)
{
  firstFrameParticles <- x[[1]]$particles
  cat("An object of the LinkedParticleList class. \n\n")
  cat("List of linked particles for",length(x),"images\n\n")
  cat("Particles were tracked throughout the subsequent",ncol(x[[1]]$nxt),"frame(s)\n\n" )
  
  cat("Displaying a subset of the features of the",nrow(firstFrameParticles),"particles found in the first image...\n")
  linesToShow <- min(5,nrow(firstFrameParticles))
  print(firstFrameParticles[1:linesToShow,1:8])
  cat("\nParticles identified on the",x[[1]]$channel,"channel\n")
}



#' initialize.ParticleList
#' 
#' Initialize a ParticleList object for subsequent linking/tracking
#'  
#' @param particlelist A ParticleList object
#' @param linkrange The number of frames to look for candidate particles potentially belonging to the same track
#' 
#' @return A ParticleList object with slots dedicated for the tracking pre-filled
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
initialize.ParticleList <- function(particlelist,
                                    linkrange=1)       
{
  out <- vector(length(particlelist),mode="list")
  class(out) <- c("ParticleList",class(out))
  # check that the number of frames is the same as the number of processed reports
  for (i in 1:length(particlelist))
  {
    out[[i]]$particles <- particlelist[[i]]$particles
    particleNr <- nrow(particlelist[[i]]$particles)
    out[[i]]$link <- rep(0,particleNr)
    out[[i]]$frame <- rep(i,particleNr)
    out[[i]]$label <- rep(NA,particleNr)
    out[[i]]$special <- rep(TRUE,particleNr)
    out[[i]]$nxt <- matrix(0,nrow=particleNr,ncol=linkrange)
    
    out[[i]]$imgSource <- particlelist[[i]]$imgSource
    out[[i]]$channel <- particlelist[[i]]$channel
  }
  return(out)
}



#' print.TrajectoryList
#' 
#' Method for displaying conveniently a TrajectoryList object
#'  
#' @param x A TrajectoryList object
#' @param ... Arguments to be passed to methods
#' 
#' @method print TrajectoryList
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' print(platelets.trajectories)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
print.TrajectoryList <- function(x,...)
{
  cat("An object of the TrajectoryList class. \n\n")
  cat("TrajectoryList composed of",length(x),"trajectories\n\n")
  
  cat("Trajectories cover a range of",max(unlist(lapply(x,function(arg){(arg$trajectory$frame)}))),"frames\n") 
  cat("Displaying a segment of the first trajectory...\n")
  print(x[[1]]$trajectory[1:min(10,nrow(x[[1]]$trajectory)),])
  
}



#' print.KinematicsFeatureSet
#' 
#' Method for displaying conveniently a KinematicsFeatureSet object
#'  
#' @param x A KinematicsFeatureSet object
#' @param ... Arguments to be passed to methods
#' 
#' @method print KinematicsFeatureSet
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' traj11features <- kinematics(platelets.trajectories,trajectoryID = 11)
#' print(traj11features)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
print.KinematicsFeatureSet <- function(x,...)
{
  cat("An object of the KinematicsFeatureSet class. \n\n")
  cat("KinematicsFeatureSet composed of",length(x) - 1,"atomic/vectorial features\n\n")
  cat("The features describe a trajectory of",length(x$delta.x) + 1,"points\n")
  
  cat("Available features:\n")
  print(names(x))
  cat("\n")
  cat("Curvilinear Velocity:",x$curvilinearVelocity,"\n")
  cat("Total Distance:",x$totalDistance,"\n")
  cat("Total Time:",x$totalTime,"\n")
  
}


#' print.KinematicsFeatureSetList
#' 
#' Method for displaying conveniently a KinematicsFeatureSetList object
#'  
#' @param x A KinematicsFeatureSetList object
#' @param ... Arguments to be passed to methods
#' 
#' @method print KinematicsFeatureSetList
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' alltrajs.features <- kinematics(platelets.trajectories)
#' print(alltrajs.features)
#'
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
print.KinematicsFeatureSetList <- function(x,...)
{
  cat("An object of the KinematicsFeatureSetList class. \n\n")
  cat("KinematicsFeatureSetList composed of",length(x)," KinematicsFeatureSet objects\n\n")
    
  cat("Available features (shown for the first trajectory):\n")
  print(names(x[[1]]))
  cat("\n")
  cat("Curvilinear Velocity:",x[[1]]$curvilinearVelocity,"\n")
  cat("Total Distance:",x[[1]]$totalDistance,"\n")
  cat("Total Time:",x[[1]]$totalTime,"\n\n")
  
  cat("Average values (calculated on",sum(unlist(lapply(x,function(arg){arg[["paramsNotComputed"]]}))),"trajectories where parameters were computed)\n")
  cat("Average Curvilinear Velocity:",mean(unlist(lapply(x,function(arg){arg[["curvilinearVelocity"]]})),na.rm=TRUE),"\n")
  cat("Average Total Distance:",mean(unlist(lapply(x,function(arg){arg[["totalDistance"]]})),na.rm=TRUE),"\n")
  cat("Average Total Time:",mean(unlist(lapply(x,function(arg){arg[["totalTime"]]})),na.rm=TRUE),"\n")
  
}


#' repmat
#' 
#' Function equivalent for MATLAB's repmat - Replicate and tile arrays
#' 
#' A more flexible and stylish alternative to replicate the behaviour of the repmat function of MATLAB
#' 
#' @param a The matrix to copy
#' @param n The n value for the tiling
#' @param m The m value for the tiling
#' 
#' @return Creates a large matrix consisting of an m-by-n tiling of copies of a. 
#' 
#' @references http://cran.r-project.org/doc/contrib/R-and-octave.txt
#' 
#' @author Robin Hankin, 2001
repmat <- function(a,n,m) 
{
  kronecker(matrix(1,n,m),a)
}
# refer to "translation document matlab to R" for credits ;)







