#' Displays with raster
#' 
#' Displays images with R native functions, makes direct use of EBImage's display
#' 
#' @param imgObject an \code{Image} object
#' @param dispMet Set to default as "raster", could be "browser"
#' @param ... Arguments to be passed to display
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
showMe <- function(imgObject, dispMet="raster",...)
{
  display(imgObject, method=dispMet)
}






#' newFrameList
#' 
#' constructor for a FrameList object
#' 
#' This function is used to create a FrameList object from a vector of images. 
#' The number of frames is also specified, as just a subset of the images can be used for this
#' 
#'@param nframes Number of frames that will constitute the FrameList object
#'@param imgsLocation Vector of strings containing the locations where the (raw) images are to be found
#'
#'@return An object of the \code{FrameList} class, which holds the info on a list of frames, specifying for each the following elements:
#'\item{image}{The \code{Image} object containing the image itself}
#'\item{location}{The complete path to the location of the original image}
#'
newFrameList <- function(nframes,
                         imgsLocation
                         )
{
  cat("Creating a new object of class FrameList...\n")
  # check that nframes coincides with the number of images available -throw an error otherwise?
  frameList <- vector(nframes,mode="list")
  class(frameList) <- c("FrameList",class(frameList))

  for (i in 1:nframes)
  {
    frameList[[i]]$image <- readImage(imgsLocation[i])
    frameList[[i]]$location <- imgsLocation[i]
    # or should i just use     frameList[[i]] <- readImage(imgsLocation[[i]])

  }
  attr(frameList,"call") <- match.call()
  cat("Created a frameList object of",nframes,"frames!\n")
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
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
print.FrameList <- function(x,...)
{
  cat("An object of the FrameList class. \n\n")
  cat("List of frames for",length(x),"images\n")
  cat("Images contain information on",ifelse(!is.na(dim(x[[1]]$image)[3]),dim(x[[1]]$image)[3],"1"),"channel(s)\n")
  cat("Image dimensions:\t",dim(x[[1]]$image)[1:2],"\n")
}




#' inspect.FrameList
#' 
#' Explore the first frames of a FrameList
#' 
#' The first frames of a FrameList are displayed in the browser, and are interactively navigable.
#' Default number of shown frames is 4, can be set maximum to 8, as this function is purely for a 
#' first inspection
#' 
#' @param framelist A FrameList object
#' @param nframes The number of frames to display
#' 
#' 
#'
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
inspect.FrameList <- function(framelist,
                             nframes=4)
{
  if(nframes > 8)
  {
    stop("Too high number of frames, try with a number <= 8")
  }
  # nframes should also be more than the nr of frames available
  cat("Displaying the first",nframes,"frames for the FrameList")
  
  firstFrames <- list()
  
  # need to set the colorMode to Grayscale if it is only one channel so that combine works!!
  
  singleChannel <- !(length(dim(framelist[[1]]$image)) > 2)
  
  firstFrames[[1]] <- framelist[[1]]$image
  if(singleChannel) 
  {
    colorMode(firstFrames[[1]]) <- Grayscale
  }
  for (i in 2:nframes)
  {
    firstFrames[[i]] <- framelist[[i]]$image
    if(singleChannel)
    {
      colorMode(firstFrames[[i]]) <- Grayscale
    }
  }
  
  firstFramesCombined <- combine(firstFrames)
  display(firstFramesCombined,all=TRUE)
}


#' fullInspection.FrameList
#' 
#' Display entirely a FrameList object
#' 
#' All frames of a FrameList are displayed in the browser, and are interactively navigable.
#' Default number of shown frames is 10, but in this case there is no upper limit to display.
#' Care should be taken with large FrameList objects
#'  
#' @param framelist A FrameList object
#' @param nframes The number of frames to display
#' 
#'
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
fullInspection.FrameList <- function(framelist,
                                     nframes=10)
{
  # nframes should also be more than the nr of frames available
  cat("Displaying the full FrameList object, composed of",length(framelist),"\n")
  
  firstFrames <- list()
  # need to set the colorMode to Grayscale if it is only one channel so that combine works!!
  
  singleChannel <- !(length(dim(framelist[[1]]$image)) > 2)
  
  firstFrames[[1]] <- framelist[[1]]$image
  if(singleChannel) 
  {
    colorMode(firstFrames[[1]]) <- Grayscale
  }
#   loopUntil <- min(nframes,length(framelist))
  for (i in 2:length(framelist))
  {
    firstFrames[[i]] <- framelist[[i]]$image
    if(singleChannel)
    {
      colorMode(firstFrames[[i]]) <- Grayscale
    }
  }
  
  firstFramesCombined <- combine(firstFrames)
  display(firstFramesCombined,all=TRUE)
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
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
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


#' createChannelsFrameList
#' 
#' Creates a ChannelsFrameList object from a FrameList object, decomposed in the acquired channels
#'  
#' @param framelist A FrameList object
#' 
#' @return A ChannelsFrameList object, which is a list of 3 FrameList objects, named respectively red, green and blue
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
createChannelsFrameList <- function(framelist)
{
  redCh <- framelist
  greenCh <- framelist
  blueCh <- framelist
  
  for (i in 1:length(framelist))
  {
    redCh[[i]]$image <- framelist[[i]]$image[,,1]
    redCh[[i]]$channel="red"
    greenCh[[i]]$image <- framelist[[i]]$image[,,2]
    greenCh[[i]]$channel <- "green"
    blueCh[[i]]$image <- framelist[[i]]$image[,,3]
    blueCh[[i]]$channel <- "blue"
  }
  
  channelsframelist <- list(red=redCh,green=greenCh,blue=blueCh)
  class(channelsframelist) <- c("ChannelsFrameList",class(channelsframelist))
  return(channelsframelist)
}






#' export.FrameList
#' 
#' Exports a FrameList object
#' 
#' Writes the images contained in the image slot of the FrameList object elements.
#' The images can be exported as single frames, or as a .gif image that is composed
#' by the single frames.
#' 
#' @param framelist A FrameList object
#' @param folder The path of the folder where the image should be written
#' @param nameStub The stub for the file name, that will be used as a prefix for the exported images
#' @param createGif Logical, whether to create or not an animated .gif file
#' @param removeAfterCreatingGif Logical, whether to remove the single exported .png images after creating the single .gif
#' 
#' @return Image files are written in the desired location
#' 
#'
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
export.FrameList <- function(framelist,
                             folder="/Users/fede/TEMP/exportFrameList/",
                             nameStub="testExport",
                             createGif=FALSE,
                             removeAfterCreatingGif=TRUE
                             )
{
  if(!file.exists(folder))
  {
    dir.create(folder,showWarnings=FALSE) # if not already existing...
  }
  imgNames <- lapply(1:length(framelist),function(arg){paste0(folder,nameStub,"_frame_",arg,".png")})
  for (i in 1:length(framelist))
  {
    writeImage(framelist[[i]]$image,imgNames[[i]])
  }
  if(createGif)
  {
    # using imagemagick
    system(paste0("convert -delay 40 ",folder,nameStub,"_frame_*.png ",folder,nameStub,".gif"))
  }
  if(removeAfterCreatingGif)
  {
    file.remove(list.files(path=folder,pattern=paste0(".png"),full.names=TRUE))
  }
}



#' newParticleList
#' 
#' 
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
newParticleList <- function()
{
  cat("need to do. Option to read in from a csv file containing preprocessed data") # would still need to fix the thing on the name of the folder containing the area
}

#' print.ParticleList
#' 
#' Method for displaying conveniently a ParticleList object
#'  
#' @param particlelist A ParticleList object
#' 
#' @method print ParticleList
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
print.ParticleList <- function(particlelist)
{
  cat("An object of the ParticleList class. \n\n")
  cat("List of particles for",length(particlelist),"images\n\n")
  cat("Displaying a subset of the features of the",nrow(particlelist[[1]]$particles),"particles found in the first image...\n")
  linesToShow <- min(5,nrow(particlelist[[1]]$particles))
  print(particlelist[[1]]$particles[1:linesToShow,1:8])
  cat("\nParticles identified on the",particlelist[[1]]$channel,"channel\n")
}

#' print.LinkedParticleList
#' 
#' Method for displaying conveniently a LinkedParticleList object
#'  
#' @param linkedparticlelist A LinkedParticleList object
#' 
#' @method print LinkedParticleList
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
print.LinkedParticleList <- function(linkedparticlelist)
{
  cat("An object of the linkedParticleList class. \n\n")
  cat("List of linked particles for",length(linkedparticlelist),"images\n\n")
  cat("Particles were tracked throughout the subsequent",ncol(linkedparticlelist[[1]]$nxt),"frame(s)\n\n" )
  
  cat("Displaying a subset of the features of the",nrow(linkedparticlelist[[1]]$particles),"particles found in the first image...\n")
  linesToShow <- min(5,nrow(linkedparticlelist[[1]]$particles))
  print(linkedparticlelist[[1]]$particles[1:linesToShow,1:8])
  cat("\nParticles identified on the",linkedparticlelist[[1]]$channel,"channel\n")
}



#' initialize.ParticleList
#' 
#' Initialize a ParticleList object for subsequent tracking
#'  
#' @param particlelist A ParticleList object
#' @param linkrange The number of frames to look for candidate particles potentially belonging to the same track
#' 
#' @return A ParticleList object with slots dedicated for the tracking pre-filled
#' 
#'
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
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
#' @param trajectorylist A TrajectoryList object
#' 
#' @method print TrajectoryList
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
print.TrajectoryList <- function(trajectorylist)
{
  cat("An object of the TrajectoryList class. \n\n")
  cat("TrajectoryList composed of",length(trajectorylist),"trajectories\n\n")
  
  cat("Trajectories cover a range of",max(unlist(lapply(trajectorylist,function(arg){(arg$trajectory$frame)}))) + 1,"frames\n") # not taking things in frame100 or smthing else happens?
  cat("Displaying a segment of the first trajectory...\n")
  print(trajectorylist[[1]]$trajectory[1:min(10,nrow(trajectorylist[[1]]$trajectory)),])
  
  
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
#'
#' 
#' @export
#' @author Robin Hankin, 2001
repmat <- function(a,n,m) 
{
  kronecker(matrix(1,n,m),a)
}
# refer to "translation document matlab to R" for credits ;)






# 
# ## old ##
# ## old ##
# ## old ##
# ## old ##
# setupFolders <- function(projectFolder="~",analysisFolder)
# {
#   setwd(projectFolder)
#   system(paste("mkdir ", analysisFolder,sep=""))
#   processingFolder <- paste(projectFolder,analysisFolder,sep="")
#   setwd(processingFolder)
#   system("mkdir segmentedImages_red")
#   system("mkdir segmentedImages_green")
#   system("mkdir processingOverviews")
#   system("mkdir thresholdOverviews")
#   system("mkdir featuresReports_red")
#   system("mkdir featuresReports_green")
#   system("mkdir paintedObjects_red")
#   system("mkdir paintedObjects_green")
#   system("mkdir paintedCells_red")
#   system("mkdir paintedCells_green")  
#   system("mkdir postprocessing_red")  
#   system("mkdir postprocessing_green")
#   system("mkdir processedReports_green")
#   system("mkdir processedReports_red")
#   system("mkdir processedReportsForMOSAIC_green")
#   system("mkdir processedReportsForMOSAIC_red")
#   system("mkdir backgroundImages")
#   system("mkdir cutoutImages")
#   system("mkdir rotatedImages")
#   
#   system("mkdir backgroundsubtractedImages_allchannels")
#   system("mkdir backgroundsubtractedImages_red")
#   system("mkdir backgroundsubtractedImages_green")
#   
#   
#   cat("Created folders for analysis in",processingFolder,"\n")
#   return(processingFolder)
# }
# 
# setupFoldersTransmigrating <- function(analysisFolder, projectFolder="~")
# {
#   setwd(projectFolder)
#   system(paste("mkdir ", analysisFolder,sep=""))
#   processingFolder <- paste(projectFolder,analysisFolder,sep="")
#   setwd(processingFolder)
#   system("mkdir segmentedImages")
#   system("mkdir processingOverviews")
#   system("mkdir thresholdOverviews")
#   system("mkdir featuresReports")
#   system("mkdir paintedObjects")
#   system("mkdir paintedCells")  
#   system("mkdir postprocessing")
#   system("mkdir processedReports")
#   
#   cat("Created folders for analysis in",processingFolder)
#   return(processingFolder)
# }
# 
# 
# ################
# calcDist <- function(frame1_obj,frame2_obj,method="euclidean")
# {
#   eudi <- sqrt((frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)^2 + (frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)^2)
#   distx <- abs(frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)
#   disty <- abs(frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)
#   
#   
#   if(method=="euclidean")
#     return(eudi)
#   if(method=="dx")
#     return(distx)
#   if(method=="dy")
#     return(disty)
# }
# 
# 
# 
# 
# 
# calcDist2 <- function(frame1_obj,frame2_obj,movementDirection="leftToRight")
# {
#   eudi <- sqrt((frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)^2 + (frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)^2)
#   distx <- abs(frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)
#   disty <- abs(frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)
#   
#   if(movementDirection=="leftToRight")
#     backDisp <- (frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)
#   if(movementDirection=="rightToLeft")
#     backDisp <- (frame2_obj$cell.0.m.cx - frame1_obj$cell.0.m.cx) 
#   if(movementDirection=="upToDown")
#     backDisp <- (frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)
#   if(movementDirection=="downToUp")
#     backDisp <- (frame2_obj$cell.0.m.cy - frame1_obj$cell.0.m.cy)
#   
#   
#   res <- list(eudi,distx,disty,backDisp)
#   names(res) <- c("euclideanDistance","xDisplacement","yDisplacement","backwardsDisplacement")
#   return(res)
#   
# }
# #################
# 
# 
# ################
# 
# ################
# listImages <- function(repositoryFolder,fullPattern=T,searchPattern="*tif")
# {
#   list.files(path=repositoryFolder, pattern=searchPattern,full.names=fullPattern)
# }
# 
# 
# ################
# # points <- locator(2,type="l",col="white",)
# 
# estimateAngle <- function()
# {
#   # to be called on a clear image
#   pstart <- locator(1,type="o",col="magenta")
#   pend <- locator(1,type="o",col="cyan")
#   arrows(pstart$x,pstart$y,pend$x,pend$y,col="white")
#   
#   cat(pend$y-pstart$y,"\t")
#   cat(pend$x-pstart$x,"\t")
#   cat((pend$y-pstart$y)/(pend$x-pstart$x),"\n")
#   
#   
#   # estAngle is the clockwise angle to rotate afterwards
#   estAngle <- atan((pend$y-pstart$y)/(pend$x-pstart$x))*180/pi
#   #   estAngle <- atan2((pend$y-pstart$y),(pend$x-pstart$x)) #*180/pi
#   #   cat(estAngle)
#   
#   quad1 <- (pend$y>=pstart$y) && (pend$x>=pstart$x)
#   quad2 <- (pend$y>=pstart$y) && (pend$x<pstart$x)
#   quad3 <- (pend$y<pstart$y) && (pend$x<pstart$x)
#   quad4 <- (pend$y<pstart$y) && (pend$x>=pstart$x)
#   cat(estAngle)
#   
#   if(quad1 | quad4) estAngle <- estAngle
#   if(quad2 | quad3) estAngle <- estAngle + 180 # + pi/2 # +180
#   
#   return(estAngle)
# }
# 
# # estA <- estimateAngle() # useless YET! - the EBImage is not able to deal with pixel coordinates interactively AND correctly..
# 
# 
# 
# # creation of the particle list of lists object - starting point for tracking algorithm
# createParticleSet <- function(nframes,
#                               particleReports,  # vector containing the location of the files for the processed reports
#                               linkrange)        # linkrange, i.e. the number of frames to look for candidate particles of the same track 
# {
#   particleSet <- vector(nframes,mode="list")
#   # check that the number of frames is the same as the number of processed reports
#   for (k in 1:length(particleReports))
#   {
#     tmpList <- list()
#     tmpList$particles <- read.delim(file=particleReports[k],sep="\t") # will contain a lot of information - position and other features computed
#     particleNr <- nrow(tmpList$particles)
#     tmpList$link <- rep(0,particleNr)
#     tmpList$frame <- rep(k,particleNr)
#     tmpList$label <- rep(NA,particleNr)
#     tmpList$special <- rep(TRUE,particleNr)
#     tmpList$nxt <- matrix(0,nrow=particleNr,ncol=linkrange)
#     particleSet[[k]] <- tmpList
#   }
#   return(particleSet)
# }
# 
# 
# 
# ## OLD
# ## OLD
# ## OLD
# ## OLD
# ## OLD
# ## OLD
# 
# ## OLD
# ## OLD
# ## OLD
# # ntrajs <- length(trajectoryList)
# createTrajectorySet <- function(trajectoryList
#                                 ) 
# {
#   ntrajs <- length(trajectoryList)
#   trajectorySet <- vector(ntrajs,mode="list")
#   
#   for (k in 1:ntrajs)
#   {
#     tmpList <- list()
#     tmpList$trajectory <- trajectoryList[[k]]
#     tmpList$npoints <- nrow(trajectoryList[[k]])
#     tmpList$nframes <- trajectoryList[[k]]$frame[nrow(trajectoryList[[k]])]-trajectoryList[[k]]$frame[1] + 1
#     tmpList$ngaps <- tmpList$nframes - tmpList$npoints
#     tmpList$keep <- NA # initialized, then set to 0 or 1
#       
#     trajectorySet[[k]] <- tmpList
#   }
#   return(trajectorySet)
# }
# 
# 
