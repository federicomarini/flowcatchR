## all Functions during development

# old read.frames
# integrated with the constructor for Frames

##################
#' Constructor for a \code{Frames} object
#' 
#' This function is used to create a \code{Frames} object from a vector of image files (or a folder specifying the directory
#' containing them). 
#' The number of frames is also specified, as just a subset of the images can be used for this
#' 
#' @param image.files Vector of strings containing the locations where the (raw) images are to be found, or alternatively, the path to the folder
#' @param nframes Number of frames that will constitute the \code{Frames} object
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
read.Frames <- function(image.files, # ../exportedMesenteriumSubset
                         nframes=NULL)
{
  cat("Creating a new object of class Frames...\n")
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
  
  if(nframes > length(image.files))
    stop("You are trying to load data for a number of frames greater than the available frames")
  
  if(length(image.files) > nframes)
    image.files <- image.files[1:nframes]
  
  multiImg <- readImage(image.files)
  # to save the image locations as dimnames # currently readImage works by saving not the full path
  dimnames(multiImg) <- list(NULL,NULL,NULL,image.files)
  y <- Frames(multiImg,channel = "all")  

  cat("Created a Frames object of",nframes,"frames.\n")
  return(y)
}




















#' Display conveniently a \code{FrameList} object
#' 
#' @param x A \code{FrameList} object
#' @param ... Arguments to be passed to methods
#'  
#' @method print FrameList
#' 
#' @examples
#' data("MesenteriumSubset")
#' print(MesenteriumSubset)
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
setMethod("show",
          signature = "Frames",
          definition = function(object){
            d <- dim(object)
            cat("Frames\n")
            cat("An object of the Frames class. \n\n")
#             cat("Multi-dimensional Image with", numberOfFrames(object,"render"),"frames\n")
#             cat("Images contain information on",ifelse(!is.na(d[3]),d[3],"1"),"channel(s)\n")
#             cat("Image dimensions:\t",d[1:2],"\n\n")
            
            cat("Displaying information for the first Image in the Frames object...\n\n")
            callNextMethod(object)
            
          })






#' Explore the first frames of a \code{FrameList}
#' 
#' The first frames of a \code{FrameList} are displayed in the browser, and are interactively navigable.
#' Default number of shown frames is 4, can be set maximum to 8, as this function is purely for a 
#' first inspection
#' 
#' @param framelist A \code{FrameList} object
#' @param nframes The number of frames to display (default value: 6)
#' @param inspectAll Logical, whether to inspect all frames (overriding the default of 10 that can be used also when \code{inspectAll} is FALSE)
#' @param display.method Method for displaying, can be either \code{raster} or \code{browser}. Defaults to \code{browser}, by opening a window in the browser
#' @param verbose Logical, whether to provide additional output on the command line alongside with the images themselves
#' 
#' @examples
#' data("MesenteriumSubset")
#' \dontrun{inspect.frames(MesenteriumSubset)}
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
inspect.Frames <- function(frames,
                           nframes=NULL,
                           display.method="browser",
                           verbose=FALSE)
{
  totframes <- length.Frames(frames)
  
  if(!is.null(nframes) && nframes < totframes)
    frames <- select.Frames(frames,1:nframes)
  
  if(is.null(nframes))
  {
    nframes <- totframes
  } 
  
  if (verbose) cat("Displaying",nframes,"frames of a Frames object composed in total of",totframes,"images")
  
  display(frames,all=TRUE,method=display.method)
}





#' Extracts subsets of frames from a \code{FrameList} object
#' 
#' An input \code{FrameList} object is subject to subsetting. This function is useful e.g. when the trajectory of interest 
#' is presenting gaps (i.e. does not actually include a frame)
#' 
#' @param x A \code{FrameList} object
#' @param framesToKeep A vector containing the indexes of the frames to keep in the selection
#' @param ... Arguments to be passed to methods
#' 
#' @return A \code{FrameList} object, composed by the subset of frames of the input \code{FrameList}
#' 
#' @method subset FrameList
#' 
#' @examples
#' data("MesenteriumSubset")
#' subset(MesenteriumSubset, framesToKeep = c(1:10, 14:20))
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014




#' @export
select.Frames <- function(frames,framesToKeep=1,...)
{
  # check if all framesToKeep are actually available in the Frames object
  totframes <- length.Frames(frames)
  if (max(framesToKeep) > totframes)
    stop("You are selecting frames that are not available in the original Frames object")
  
  
  y <- combine(lapply(framesToKeep,function(i) getFrame(frames,i,type = "render")))
#   y <- Frames(multiImg,channel=frames@channel)

  return(y)
}





#' Creates a \code{ChannelsFrameList} object from a \code{FrameList} object, decomposed in the acquired channels
#'  
#' @param framelist A \code{FrameList} object
#' 
#' @return A \code{ChannelsFrameList} object, which is a list of 3 \code{FrameList} objects, named respectively \code{red}, \code{green} and \code{blue}
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
# channels <- function(framelist)
# {
#   # check if the framelist indeed has more than one channel, first of all! -> needs to be done TODO
#   redCh <- framelist
#   greenCh <- framelist
#   blueCh <- framelist
#   
#   for (i in 1:length(framelist))
#   {
#     redCh[[i]]$image <- channel(framelist[[i]]$image,"red")
#     redCh[[i]]$channel="red"
#     greenCh[[i]]$image <- channel(framelist[[i]]$image,"green")
#     greenCh[[i]]$channel <- "green"
#     blueCh[[i]]$image <- channel(framelist[[i]]$image,"blue")
#     blueCh[[i]]$channel <- "blue"
#   }
#   
#   channelsframelist <- list(red=redCh,green=greenCh,blue=blueCh)
#   class(channelsframelist) <- c("ChannelsFrameList",class(channelsframelist))
#   return(channelsframelist)
# }


length.Frames <- function(frames)
{
  numberOfFrames(frames,"render")
}


#' Exports a \code{FrameList} object
#' 
#' Writes the images contained in the \code{image} slot of the \code{FrameList} object elements.
#' The images can be exported as single frames, or as a .gif image that is composed
#' by the single frames.
#' 
#' @param framelist A \code{FrameList} object
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
export.Frames <- function(frames,
                          dir=tempdir(),
                          nameStub="testExport",
                          createGif=FALSE,
                          removeAfterCreatingGif=TRUE)
{
  if(!file.exists(dir))
  {
    dir.create(dir,showWarnings=FALSE) # if not already existing...
  }
  totframes <- length.Frames(frames)
  imgNames <- unlist(lapply(1:totframes,
                     function(arg){paste0(dir,"/",nameStub,"_frame_",formatC(arg,nchar(length.Frames(Frames)),flag="0"),".png")}))
  writeImage(frames,imgNames)
  
  if(createGif)
  {
    # using imagemagick
    system(paste0("convert -delay 40 ",dir,"/",nameStub,"_frame_*.png ",dir,"/",nameStub,".gif"))
  }
  if(removeAfterCreatingGif && createGif)
  {
    file.remove(list.files(path=dir,pattern=paste0(".png"),full.names=TRUE))
  }
  invisible()
}





#' Exports a \code{ParticleList} object
#' 
#' Writes the particles contained in the \code{particles} data frame slot of the \code{ParticleList} object elements.
#' A track of the provenience of the particles is stored as a comment line above the header
#' 
#' @param particlelist A \code{ParticleList} object
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
export.particles <- function(particleset,
                             dir=tempdir(),
                             nameStub="testExport_particles")
{
  cat("Exporting the .tsv files for the ParticleSet...\n")
  if(!file.exists(dir))
  {
    dir.create(dir,showWarnings=FALSE) # if not already existing...
  }
  particleNames <- lapply(1:length(particleset),
                          function(arg){paste0(dir,"/",nameStub,"_frame_",formatC(arg,nchar(length(particleset)),flag="0"),".tsv")})
  for (i in 1:length(particleset))
  {
    writeLines(paste0("#|",names(particleset)[i],"|",particleset@channel),particleNames[[i]])
    write.table(particleset@.Data[[i]],particleNames[[i]],append = TRUE,sep = "\t",quote = FALSE,col.names = TRUE,row.names= FALSE)
  }
  cat("Done exporting the .tsv files for the ParticleSet.\n
      You can ignore the warning messages as long as you remind the additional comment line added starting with '#'")
  invisible()
}



#' Constructor for a \code{ParticleList} object
#' 
#' This function is used to create a \code{ParticleList} object from a vector/list of tab separated text files, each of one containing one line for each 
#' particle in the related frame, alongside with its coordinates and if available, the computed features
#' The number of frames is also specified, as just a subset of the particle lists can be used for this
#' 
#' @param particle.files Vector of strings containing the locations where the particle coordinates are to be found, or alternatively, the path to the folder
#' @param nframes Number of frames that will constitute the \code{ParticleList} object
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
  cat("Creating a new object of class ParticleSet...\n")
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
#   particleList <- vector(nframes,mode="list")
#   class(particleList) <- c("ParticleList",class(particleList))
# 
  importedChannel <- unlist(strsplit(readLines(particle.files[1],n=1),split = "|",fixed=TRUE))[3]
  out <- ParticleSet(channel = importedChannel)
  
  for (i in 1:nframes)
  {
    imgSource <- unlist(strsplit(readLines(particle.files[i],n=1),split = "|",fixed=TRUE))[2]
    out[[imgSource]] <- read.table(particle.files[i],sep="\t",comment.char = "#",header=TRUE,stringsAsFactors = FALSE)
#     commentLine <- readLines(particle.files[i],n=1)
#     particleList[[i]]$imgSource <- unlist(strsplit(commentLine,split = "|",fixed=TRUE))[2]
#     particleList[[i]]$channel <- unlist(strsplit(commentLine,split = "|",fixed=TRUE))[3]
  }
  #   attr(particleList,"call") <- match.call()
  cat("Created a ParticleSet object of",nframes,"frames.\n")
  return(out)
}


#' Display conveniently a \code{ParticleList} object
#'  
#' @param x A \code{ParticleList} object
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
setMethod("show",
          signature = "ParticleSet",
          definition = function(object){
            firstFrameParticles <- object[[1]]
            cat("An object of the ParticleSet class. \n\n")
            cat("Set of particles for",length(object),"images\n\n")
            cat("Displaying a subset of the features of the",nrow(firstFrameParticles),"particles found in the first image...\n")
            linesToShow <- min(5,nrow(firstFrameParticles))
            print(firstFrameParticles[1:linesToShow,1:8])
            cat("\nParticles identified on the",object$channel,"channel\n")          
          })


#' Display conveniently a \code{LinkedParticleList} object
#'  
#' @param x A \code{LinkedParticleList} object
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
setMethod("show",
          signature = "LinkedParticleSet",
          definition = function(object){
            firstFrameParticles <- object[[1]]
            cat("An object of the LinkedParticleSet class. \n\n")
            cat("Set of particles for",length(object),"images\n\n")
            cat("Particles are tracked throughout the subsequent",ncol(object@tracking[[1]]$nxt),"frame(s)\n\n" )
            
            cat("Displaying a subset of the features of the",nrow(firstFrameParticles),"particles found in the first image...\n")
            linesToShow <- min(5,nrow(firstFrameParticles))
            print(firstFrameParticles[1:linesToShow,1:8])
            cat("\nParticles identified on the",object@channel,"channel\n")          
          })





#' Initialize a \code{ParticleList} object for subsequent linking/tracking
#'  
#' @param particlelist A ParticleList object
#' @param linkrange The number of frames to look for candidate particles potentially belonging to the same track
#' 
#' @return A ParticleList object with slots dedicated for the tracking pre-filled
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
initialize.LinkedParticleSet <- function(particleset,
                                    linkrange=1)       
{
  out <- new("LinkedParticleSet",
             .Data = particleset@.Data,
             channel = particleset@channel)
  # for the tracking slot...
  tmp <- vector(length(particleset),mode="list")
  for (i in 1:length(particleset))
  {
    particleNr <- nrow(particleset[[i]])
    tmp[[i]]$link <- rep(0,particleNr)
    tmp[[i]]$frame <- rep(i,particleNr)
    tmp[[i]]$label <- rep(NA,particleNr)
    tmp[[i]]$special <- rep(TRUE,particleNr)
    tmp[[i]]$nxt <- matrix(0,nrow=particleNr,ncol=linkrange)
    
    #     out[[i]]$imgSource <- particlelist[[i]]$imgSource
    #     out[[i]]$channel <- particlelist[[i]]$channel
  }
  out@tracking <- tmp

  return(out)
}






#' Display conveniently a \code{TrajectoryList} object
#'  
#' @param x A \code{TrajectoryList} object
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

setMethod("show",
          signature = "TrajectorySet",
          definition = function(object){
            cat("An object of the TrajectorySet class. \n\n")
            cat("TrajectorySet composed of",length(object),"trajectories\n\n")
            
            cat("Trajectories cover a range of",max(unlist(lapply(object@.Data,function(arg){(arg$trajectory$frame)}))),"frames\n") 
            cat("Displaying a segment of the first trajectory...\n")
            print(object[[1]]$trajectory[1:min(10,nrow(object[[1]]$trajectory)),])
            
            cat("\nTrajectories are related to articles identified on the",object@channel,"channel\n")          
          })



#' Displaying conveniently a \code{KinematicsFeatureSet} object
#'  
#' @param x A \code{KinematicsFeatureSet} object
#' @param ... Arguments to be passed to methods
#' 
#' @method print KinematicsFeatureSet
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' traj11features <- kinematics(platelets.trajectories,trajectoryIDs = 11)
#' print(traj11features)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# print.KinematicsFeatureSet <- function(x,...)
# {
#   cat("An object of the KinematicsFeatureSet class. \n\n")
#   cat("KinematicsFeatureSet composed of",length(x) - 1,"atomic/vectorial features\n\n")
#   cat("The features describe a trajectory of",length(x$delta.x) + 1,"points\n")
#   
#   cat("Available features:\n")
#   print(names(x))
#   cat("\n")
#   cat("Curvilinear Velocity:",x$curvilinearVelocity,"\n")
#   cat("Total Distance:",x$totalDistance,"\n")
#   cat("Total Time:",x$totalTime,"\n")
#   
# }
setMethod("show",
          signature = "KinematicsFeatures",
          definition = function(object){
            cat("An object of the KinematicsFeatures class. \n\n")
            cat("KinematicsFeatures composed of",length(object) - 1,"atomic/vectorial features\n\n")
            cat("The features describe a trajectory of",length(object$delta.x) + 1,"points\n")
            
            cat("Available features:\n")
            print(names(object))
            cat("\n")
            cat("Curvilinear Velocity:",object$curvilinearVelocity,"\n")
            cat("Total Distance:",object$totalDistance,"\n")
            cat("Total Time:",object$totalTime,"\n")          
          })


#' Display conveniently a \code{KinematicsFeatureSetList} object
#'  
#' @param x A \code{KinematicsFeatureSetList} object
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
setMethod("show",
          signature = "KinematicsFeaturesSet",
          definition = function(object){
            cat("An object of the KinematicsFeaturesSet class. \n\n")
            cat("KinematicsFeaturesSet composed of",length(object)," KinematicsFeatures objects\n\n")
            
            cat("Available features (shown for the first trajectory):\n")
            print(names(object[[1]]))
            cat("\n")
            cat("Curvilinear Velocity:",object[[1]]$curvilinearVelocity,"\n")
            cat("Total Distance:",object[[1]]$totalDistance,"\n")
            cat("Total Time:",object[[1]]$totalTime,"\n\n")
            
            cat("Average values (calculated on",sum(unlist(lapply(object,function(arg){arg[["paramsNotComputed"]]}))),"trajectories where parameters were computed)\n")
            cat("Average Curvilinear Velocity:",mean(unlist(lapply(object,function(arg){arg[["curvilinearVelocity"]]})),na.rm=TRUE),"\n")
            cat("Average Total Distance:",mean(unlist(lapply(object,function(arg){arg[["totalDistance"]]})),na.rm=TRUE),"\n")
            cat("Average Total Time:",mean(unlist(lapply(object,function(arg){arg[["totalTime"]]})),na.rm=TRUE),"\n")
            
          })



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







#################### newfile ######################
#' flowcatchR: analyzing time-lapse microscopy imaging, from detection to tracking
#'
#' A toolset to analyze in vivo microscopy imaging data focused on tracking
#' flowing blood cells.
#' 
#' flowcatchR is a set of tools to analyze in vivo microscopy imaging
#' data, focused on tracking flowing blood cells. It guides the steps from
#' segmentation to calculation of features, filtering out particles not of
#' interest, providing also a set of utilities to help checking the quality of
#' the performed operations (e.g. how good the segmentation was). The main
#' novel contribution investigates the issue of tracking flowing cells such as
#' in blood vessels, to categorize the particles in flowing, rolling and
#' adherent. This classification is applied in the study of phenomena such as
#' hemostasis and study of thrombosis development.
#' 
#' @import EBImage
#' @import rgl
#' @import colorRamps
#' @import methods
#' 
#' @author
#' Federico Marini \email{marinif@@uni-mainz.de},
#' Johanna Mazur \email{mazur@@uni-mainz.de},
#' Harald Binder \email{binderh@@uni-mainz.de}
#'
#' Maintainer: Federico Marini \email{marinif@@uni-mainz.de}
#' @name flowcatchR
#' @docType package
NULL




#' A sample \code{FrameList object} 
#' 
#' The sample \code{FrameList} object is constituted by a subset of a time-lapse intravital microscopy imaging dataset.
#' Green channel marks leukocytes, red channel focuses on blood platelets. 20 frames are provided in this subset.
#' Images are kindly provided by Sven Jaeckel (\email{Sven.Jaeckel@@unimedizin-mainz.de}).
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
#' @name MesenteriumSubset
#' @docType data
NULL



#' A sample \code{ParticleList} object
#' 
#' The sample \code{ParticleList} object is constituted by the platelets identified from the \code{MesenteriumSubset} data
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
#' @name candidate.platelets
#' @docType data
NULL



# .FLOWCATCHR_VERSION <- '0.99.1'
# #'
# #'
# .onAttach <- function(lib, pkg, ...) {
# packageStartupMessage(sprintf("\nThis is flowcatchR version %s - A toolset to analyze in vivo microscopy imaging data
# for tracking flowing blood cells. Copyright (C) 2014 Federico Marini\n
# Type '?flowcatchR' for help or see www.imbei.de for more details", .FLOWCATCHR_VERSION))
# }




#################### newfile ######################

#' Calculate a set of kinematics parameters from a single trajectory
#' 
#' The computed set of parameters include \code{delta.x}, \code{delta.t} and \code{delta.v}
#' (displacements and instantaneous velocity), \code{totalTime}, \code{totalDistance},
#' \code{distStartToEnd}, \code{curvilinearVelocity}, \code{straightLineVelocity} and
#' \code{linearityForwardProgression}, Mean Squared Displacement, velocity
#' autocorrelation, and more
#' 
#' @param trajectorylist A \code{TrajectoryList} object
#' @param trajectoryID The ID of a single trajectory
#' @param acquisitionFrequency The frame rate of acquisition for the images, in
#'   milliseconds
#' @param scala The value of micro(?)meters to which each single pixel
#'   corresponds
#'   
#' @return A \code{KinematicsFeatureSet} object
#'   
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
extractKinematics.traj <- function(trajectoryset,
                                   trajectoryID,
                                   acquisitionFrequency=30, # in milliseconds
                                   scala=50 # 1 pixel is ... micrometer
)
{
  singleTraj <- trajectoryset[[trajectoryID]]$trajectory
  # throw a warning/message/error if the traj is below 3 points
  if(nrow(singleTraj) < 4)
  {
    warning(paste0("The trajectory with ID ",trajectoryID," had 3 or less points, no features were computed."))
    out <- list(delta.x=NA,
                delta.t=NA,
                delta.v=NA,
                totalTime=NA,
                totalDistance=NA,
                distStartToEnd=NA,
                curvilinearVelocity=NA,
                straightLineVelocity=NA,
                linearityForwardProgression=NA,
                trajMSD=NA,
                velocityAutoCorr=NA,
                instAngle=NA,
                directChange=NA,
                dirAutoCorr=NA,
                paramsNotComputed=TRUE
    )
#     class(out) <- c("KinematicsFeatureSet",class(out))
    y <- new("KinematicsFeatures",out)
    return(y)
  }
  sx <- singleTraj$xCoord
  sy <- singleTraj$yCoord
  d1x <- diff(sx)
  d1y <- diff(sy)
  polarCoord <- toPolarCoords(d1x,d1y) # if needed at all
  
  delta.x <- sqrt(d1x^2 + d1y^2)
  delta.t <- diff(singleTraj$frame) * acquisitionFrequency # should account for gaps
  
  totalTime <- sum(delta.t)  
  totalDistance <- sum(delta.x)
  delta.v <- delta.x/delta.t # * scala # to convert from pixel/t to the actual measurement unit
  curvilinearVelocity <- totalDistance/totalTime # = mean(delta.v) if no gaps are there
  
  distStartToEnd <- sqrt( (sx[length(sx)] - sx[1])^2 + (sy[length(sy)] - sy[1])^2 )
  straightLineVelocity <- distStartToEnd / totalTime
  
  linearityForwardProgression <- straightLineVelocity / curvilinearVelocity
  
  # msd only to compute where no gaps are present?
  trajMSD <- computeMSD(sx,sy,until=floor(nrow(singleTraj)/4))
  velocityAutoCorr <- acf(delta.v,plot=FALSE)
  # 
  
  # directionality of the traj, with angles
  instAngle <- atan2(d1y,d1x)
  directChange <- diff(instAngle)
  dirAutoCorr <- acf(directChange,plot=FALSE)
  
  out <- list(delta.x=delta.x,
              delta.t=delta.t,
              delta.v=delta.v,
              totalTime=totalTime,
              totalDistance=totalDistance,
              distStartToEnd=distStartToEnd,
              curvilinearVelocity=curvilinearVelocity,
              straightLineVelocity=straightLineVelocity,
              linearityForwardProgression=linearityForwardProgression,
              trajMSD=trajMSD,
              velocityAutoCorr=velocityAutoCorr,
              instAngle=instAngle,
              directChange=directChange,
              dirAutoCorr=dirAutoCorr,
              paramsNotComputed=FALSE
  )
#   class(out) <- c("KinematicsFeatureSet",class(out))
  y <- new("KinematicsFeatures",out)
  return(y)
}



#' Calculate a set of kinematics parameter from a \code{TrajectoryList} object, or a single parameter, or from a single trajectory (all possible combinations)
#' 
#' The computed set of parameters include \code{delta.x}, \code{delta.t} and \code{delta.v}
#' (displacements and instantaneous velocity), \code{totalTime}, \code{totalDistance},
#' \code{distStartToEnd}, \code{curvilinearVelocity}, \code{straightLineVelocity} and
#' \code{linearityForwardProgression}, Mean Squared Displacement, velocity
#' autocorrelation, and more. If a single trajectory is specified, the computation is
#' performed for that trajectory alone. If a parameter is specified, only that
#' parameter is reported, either for one or all trajectories
#' 
#' @param trajectorylist A \code{TrajectoryList} object
#' @param trajectoryIDs The ID of a single trajectory
#' @param acquisitionFrequency The frame rate of acquisition for the images, in
#'   milliseconds
#' @param scala The value of micro(?)meters to which each single pixel
#'   corresponds
#' @param feature Character string, the name of the feature to be computed
#'   
#' @return A \code{KinematicsFeatureSetList} object, or a \code{KinematicsFeatureSet} object,
#'   or an atomic value, or a list(eventually coerced to a vector)
#'   
#' @examples
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' # for all trajectories, all features
#' alltrajs.features <- kinematics(platelets.trajectories)
#' # for one trajectory, all features
#' traj11features <- kinematics(platelets.trajectories,trajectoryIDs = 11)
#' # for all trajectories, one feature
#' alltrajs.curvVel <- kinematics(platelets.trajectories,feature = "curvilinearVelocity")
#'   
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
kinematics <- function(trajectoryset,
                       trajectoryIDs=NULL,
                       acquisitionFrequency=30, # in milliseconds
                       scala=50, # 1 pixel is ... micrometer
                       feature=NULL)
{
  # first check whether the user wants to know what features are available to compute
  if(!is.null(feature)) {
    if (feature=="?") {
      cat("Available features to compute are listed here below.
          Please select one among delta.x, delta.t, delta.v, totalTime,
          totalDistance, distStartToEnd, curvilinearVelocity,
          straightLineVelocity, linearityForwardProgression, trajMSD or velocityAutoCorr")
      return(NULL)
    }
  }
  # compute all, for one or all trajectories
  if(!is.null(trajectoryIDs)) # then operate on a single trajectory
  {
    
    kineSet.out <- extractKinematics.traj(trajectoryset,trajectoryIDs,acquisitionFrequency=acquisitionFrequency,scala=scala) # returns a KinematicsFeatureSet object
    # then eventually just report the desired kinematic feature
    if(is.null(feature))
    {
      return(kineSet.out)
    } else {
      kineFeat.out <- kineSet.out[[feature]]
      if(is.null(kineFeat.out)) stop("You selected a feature whose name is not available in the set (or was not computed because the trajectory was too short).
                                     Please select one among delta.x, delta.t, delta.v, totalTime, totalDistance, distStartToEnd, curvilinearVelocity, straightLineVelocity, 
                                     linearityForwardProgression, trajMSD or velocityAutoCorr")
      return(kineFeat.out)
    }
    
  } else { # it will be done on all trajectories of the trajectorylist
    tmp <- list()
    for(i in 1:length(trajectoryset))
    {
      kineOne <- extractKinematics.traj(trajectoryset,i,acquisitionFrequency=acquisitionFrequency,scala=scala) #, acquisitionFrequency=30, # in milliseconds, scala=50
      tmp[[length(tmp) + 1]] <- kineOne
    }
    # additional class attribute?
    #   class(kineSetList.out) <- c("KinematicsFeatureSetList",class(kineSetList.out)) # it is a list of KinematicsFeatureSet objects
    kineSetList.out <- new("KinematicsFeaturesSet",tmp)
    # then eventually just report the desired kinematic feature
    if(is.null(feature))
    {
      return(kineSetList.out)
    } else {
      kineFeatList.out <- lapply(kineSetList.out,function(arg){arg[[feature]]})
      if(all(unlist(lapply(kineFeatList.out,function(arg){is.null(arg)})))) stop("You selected a feature whose name is not available in the set (or was not computed because
                                                                                 the trajectory was too short). Please select one among delta.x, delta.t, delta.v, totalTime,
                                                                                 totalDistance, distStartToEnd, curvilinearVelocity, straightLineVelocity, 
                                                                                 linearityForwardProgression, trajMSD or velocityAutoCorr")
      if(feature %in% c("totalTime","totalDistance","distStartToEnd","curvilinearVelocity","straightLineVelocity","linearityForwardProgression"))
      {
        kineFeatList.out <- unlist(kineFeatList.out)
      }
      return(kineFeatList.out)
    }
  }
}






#' Calculates the Mean Squared Displacement for a trajectory
#'  
#' @param sx x axis positions along the trajectory
#' @param sy y axis positions along the trajectory
#' @param until how many points should be included in the Mean Squared Displacement curve
#' 
#' @return A numeric vector containing the values of the MSD
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
computeMSD <- function(sx,sy,until=4)
{
  msd.t <- rep(0,until)
  for (dt in 1:until)
  {
    displacement.x <- as.vector(na.omit(sx[(1+dt):length(sx)]) - sx[1:(length(sx)-dt)])
    displacement.y <- as.vector(na.omit(sy[(1+dt):length(sy)]) - sy[1:(length(sy)-dt)])
    sqrdispl <- (displacement.x^2 + displacement.y^2)
    msd.t[dt] <- mean(sqrdispl)
  }
  return(msd.t)
}




#' Converts cartesian coordinates to polar coordinates
#' 
#' Conversion from (x,y) to (radius,theta)
#'  
#' @param x x coordinate
#' @param y y coordinate
#' 
#' @return A list containing Theta and Radius, as in polar coordinates
#'  
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
toPolarCoords <- function(x,y)
{
  Theta <- atan2(y,x)
  Radius <- sqrt(x^2 + y^2)
  return(list(Theta=Theta,Radius=Radius))
}



#' Converts polar coordinates to cartesian coordinates
#' 
#' Conversion from (radius,theta) to (x,y)
#'  
#' @param Theta The Theta angle
#' @param Radius The radius value in polar coordinates
#' 
#' @return A list containing Theta and Radius, as in polar coordinates
#'  
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
toCartesianCoords <- function(Theta,Radius)
{
  xCoord <- Radius * cos(Theta)
  yCoord <- Radius * sin(Theta)
  return(list(x=xCoord,y=yCoord))
}





#################### newfile ######################


#' Combines the information from a raw \code{FrameList} object and the corresponding preprocessed one
#' 
#' All objects are painted with a unique colour - for sake of speed
#'  
#' @param rawframelist A \code{FrameList} object containing the raw images
#' @param preprocessedframelist A \code{FrameList} object with the preprocessed versions of the images (e.g. segmented)
#' @param col A color character string, to select which color will be used for drawing the contours of the particles. If not specified, it will default according to the objects provided
#' 
#' @return A \code{FrameList} object, whose images are the combination of the raw images with the segmented objects drawn on them
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
addParticles <- function(raw.frames,binary.frames,col=NULL)
{
#   out <- vector("list",length(rawframelist))
  tmpFL <- list()
  for (i in 1:length.Frames(raw.frames))
  {
    rawimg <- Image(getFrame(raw.frames,i,"render"))
    segmimg <- Image(getFrame(binary.frames,i,"render"))
    
    if(is.null(col)) col <- "yellow"
    rawWithObj <- paintObjects(segmimg,rawimg,col=col)
    
    tmpFL[[i]] <- rawWithObj
  }
  out <- Frames(combine(tmpFL),channel="all")
  return(out)
  
}



# #' Combines raw and segmented images
# #'  
# #' @param rawimg An \code{Image} object with the raw frame data
# #' @param segmimg An \code{Image} object with the segmented objects
# #' 
# #' @return An \code{Image} object that combines raw and segmented images, with objects painted singularly with different colours
# #' 
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# actionPaint <- function(rawimg,segmimg)
# {
#   buildingUp <- rawimg
#   for(obj in 1:max(segmimg))
#   {
#     elab_singleObj <- segmimg
#     elab_singleObj[segmimg!=obj] <- 0
#     builtUp <- paintObjects(elab_singleObj,buildingUp,col=colors()[52+obj])
#     buildingUp <- builtUp
#   }
#   return(builtUp)
# }







#' Combines the information from a raw \code{FrameList} object and the corresponding preprocessed one,
#' but this time every object is painted with a different colour
#' 
#' Every object is now shown with a different colour. Care should be taken, as this function is rather slower
#' than combine.preprocessedFrameList
#'   
#' @param rawframelist A \code{FrameList} object containing the raw images
#' @param preprocessedframelist A \code{FrameList} object with the preprocessed versions of the images (e.g. segmented)
#' 
#' @return A \code{FrameList} object, whose images are the combination of the raw images with the segmented objects drawn on them,
#' painted singularly with different colours
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# combineWcolor.preprocessedFrameList <- function(rawframelist,preprocessedframelist) 
#   # careful, it's kind of slower for painting all single objects separately and of different colors!
#   # currently works best only when input raw image is in colorMode Color - as the colours there have actually a meaning 
#   # maybe do a function actionPaint that actually is internally called here - params: rawimg, segmimg -> DONE
# {
#   out <- vector("list",length(rawframelist))
#   
#   if(length(dim(rawframelist[[1]]$image))<3)
#     warning("You are trying to paint coloured cells on a single channel - info will be not so useful, try instead combine.preprocessedFrameList, it should be much faster.")
#   
#   for (i in 1:length(out))
#   {
#     rawimg <- rawframelist[[i]]$image
#     segmimg <- preprocessedframelist[[i]]$image
#     
#     # works well if rawimg has still all 3 frames, otherwise i guess it stays B/W
#     if(length(dim(rawimg))>2)
#     {
#       rawWithObj <- actionPaint(rawimg,segmimg)    
#     } else {
#       channel <- rawframelist[[i]]$channel
#       
#       switch(channel,
#              red={
#                rawWithObj <- rgbImage(red=actionPaint(rawimg,segmimg))
#              }, 
#              green={
#                rawWithObj <- rgbImage(green=actionPaint(rawimg,segmimg))
#              },
#              blue={
#                rawWithObj <- rgbImage(blue=actionPaint(rawimg,segmimg))
#              },
#              stop("No channel value was stored in the appropriate slot!")
#       )
#     }
#     out[[i]]$image <- rawWithObj
#   }
#   
#   class(out) <- c("FrameList",class(out))
#   return(out)
#   
# }










#################### newfile ######################
#' Cut borders of a \code{FrameList} object
#' 
#' Performs cropping on the \code{FrameList} object, selecting how many pixels should be cut on each side
#' 
#' Cropping can be performed with careful choice of all cutting sides, or cropping a single value from
#' all sides
#' 
#' @param x An input \code{FrameList} object
#' @param cutLeft Amount of pixels to be cut at the side
#' @param cutRight Amount of pixels to be cut at the side
#' @param cutUp Amount of pixels to be cut at the side
#' @param cutDown Amount of pixels to be cut at the side
#' @param cutAll Amount of pixels to be cut at all sides. Overrides the single side values
#' @param testing Logical, whether to just test the cropping or to actually perform it. Default set to \code{FALSE}
#' @param ... Arguments to be passed to methods
#' 
#' @return A \code{FrameList} object, with cropped frames in the \code{image} slot
#' 
#' @examples 
#' data("MesenteriumSubset")
#' cut(MesenteriumSubset)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
crop.Frames <- function(frames,
                          cutLeft=5,cutRight=5,cutUp=5,cutDown=5,
                          cutAll=0,
                          testing=FALSE,
                          ...)
{
#   out <- vector(length(x),mode="list")
#   class(out) <- c("FrameList",class(out))
  tmpFL <- vector(length.Frames(frames),mode="list")
  if(cutAll > 0)
  {
    cutLeft <- cutRight <- cutUp <- cutDown <- cutAll
  }
  
  if(!testing)
  {
    for(i in 1:length.Frames(frames))
    {
      img <- getFrame(frames,i,"render")
      if (numberOfFrames(img)==3)
        cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
      else
        cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown)]
      
      tmpFL[[i]] <- cutoutImg
    }
    out <- combine(tmpFL)
    return(out)
  } else {
    # just check on one image, the first one
    img <- getFrame(frames,i,"render")
    if (numberOfFrames(img)==3)
      cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
    else
      cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown)]
    display(cutoutImg)
    return(cutoutImg)
  }
}



#' Rotates all images in a \code{FrameList} object 
#' 
#' Rotation is performed exploiting the rotate function of the \code{EBImage} package. Could be automated if support for coordinate/pixel interaction is included
#' 
#' @param framelist A \code{FrameList} object
#' @param rotAngle The rotation angle (clockwise) specified in degrees
#' @param testing Logical, whether to just test the rotation or to actually perform it. Default set to \code{FALSE}
#' @param output.origin A vector of 2 numbers indicating the dimension of the output image, as in the rotate function
#' @param output.dim A vector of 2 numbers indicating the output coordinates of the origin in pixels, as in the \code{rotate} function
#'  
#' @return A \code{FrameList} object containing the rotated frames
#' 
#' @examples 
#' data("MesenteriumSubset")
#' rotate.FrameList(MesenteriumSubset,rotAngle = 40)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
rotate.Frames <- function(frames,
                          angle,
                          testing=FALSE,
                          output.origin=c(dim(frames)[1]/3,dim(frames)[2]/3),
                          output.dim=c(dim(frames)[1]*1.5,dim(frames)[2]*1.5))
{
  if(!testing)
  {
    y <- frames
    y <- rotate(y, angle = angle, output.origin=output.origin, output.dim=output.dim)
    return(y)
  } else {
    # just check
    
    display(rotate(frames, angle = angle, output.origin=output.origin, output.dim=output.dim))
    invisible()
  }  
}












#' @export
channel.Frames <- function(frames,mode)
{
  y <- Frames(EBImage::channel(frames,mode),channel="all")
  y@channel<-mode
  return(y)
}




#' Preprocessing images
#' 
#' Can be applied to \code{FrameList} or \code{ChannelsFrameList} objects. \code{ChannelsFrameList} are then subset to the chosen channel,
#' and the method \code{preprocess.FrameList} is then applied, with its set of parameters
#'  
#' @param x A \code{FrameList} or a \code{ChannelsFrameList} object
#' @param ... Arguments to be passed to methods, such as channel and/or preprocessing parameters
#' 

#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
#' #' Preprocessing function for \code{ChannelsFrameList} objects
#' 
#' \code{ChannelsFrameList} are then subset to the chosen channel, and the method \code{preprocess.FrameList} is then applied, with its set of parameters
#'  
#' @param x A \code{ChannelsFrameList} object
#' @param channel Character string. The channel to perform the operations on. Can be \code{red}, \code{green} or \code{blue}
#' @param ... Arguments to be passed to methods
#' 
#' @return A \code{FrameList} object, whose frame images are the preprocessed versions of the input images
#' 
#' @examples
#' data("MesenteriumSubset")
#' preprocess(channels(MesenteriumSubset),channel = "red")
#' 

#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
#' Preprocessing function for \code{FrameList} objects
#' 
#' \code{FrameList} objects are processed according to the chosen set of parameters. Many of them refer directly to 
#' existing \code{EBImage} functions, please see the corresponding help for additional information
#'  
#' @param x A \code{FrameList} object
#' @param brush.size Size in pixels of the brush to be used for initial smoothing
#' @param brush.shape Shape of the brush to be used for initial smoothing
#' @param at.offset Offset to be used in the adaptive thresholding step
#' @param at.wwidth Width of the window for the adaptive thresholding step
#' @param at.wheight Height of the window for the adaptive thresholding step
#' @param kern.size Size in pixels of the kernel used for morphological operations
#' @param kern.shape Shape of the kernel used for morphological operations
#' @param ws.tolerance Tolerance allowed in performing the watershed-based segmentation
#' @param ws.radius Radius for the watershed-based segmentation
#' @param displayprocessing Logical, whether to display intermediate steps while performing preprocessing. Dismissed currently, it could increase runtime a lot
#' @param ... Arguments to be passed to methods
#' 
#' @return A \code{FrameList} object, whose frame images are the preprocessed versions of the input images
#' 
#' @examples
#' data("MesenteriumSubset")
#' platelets.framelist <- channels(MesenteriumSubset)$red
#' preprocess(platelets.framelist)
#' 

#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
#' 
#' 

#' @export
preprocess.Frames <- function(frames,
                        brush.size=3,
                        brush.shape="disc",
                        at.offset=0.15,
                        at.wwidth=10,
                        at.wheight=10,
                        kern.size=3,
                        kern.shape="disc",
                        ws.tolerance=1,
                        ws.radius=1,
                        displayprocessing=FALSE,
                        ...) # for the single channel images/for one channel of multi-channel images
{
  if(frames@channel=="all")
    stop("Please select a channel to work on, run channel.Frames on the Frames object you provided")
  
  flo = makeBrush(brush.size, brush.shape, step=FALSE)^2
  flo <- flo/sum(flo)
  # imageData(frames) is storing the raw Images
  thresh_img <- thresh(filter2(imageData(frames),flo),w=at.wwidth,h=at.wheight,offset=at.offset)
  
  # if needed with a step of smoothing & co (operations of opening,...)
  kern <- makeBrush(size=kern.size,shape=kern.shape)
  
  distmap_thre <- distmap(thresh_img)
  watershed_thre <- watershed(distmap_thre,tolerance=ws.tolerance,ext=ws.radius) 
  
  # just replace the image data
  imageData(frames) = watershed_thre
  return(frames)
}


# preprocess.ChannelsFrameList <- function(x,
#                                          channel="",
#                                          ...) 
# {
#   switch(channel,
#          red={
#            cat("Preprocessing the red channel...\n")
#            out <- preprocess.FrameList(x[[1]])
#          }, 
#          green={
#            cat("Preprocessing the green channel...\n")
#            out <- preprocess.FrameList(x[[2]])
#          },
#          blue={
#            cat("Preprocessing the blue channel...\n")
#            out <- preprocess.FrameList(x[[3]])
#          },
#          stop("You did not choose any of the value for the channel - allowed values= red | green | blue")
#   )
#   return(out)
# }


# preprocess.FrameList <- function(x,
#                                  brush.size=3,
#                                  brush.shape="disc",
#                                  at.offset=0.15,
#                                  at.wwidth=10,
#                                  at.wheight=10,
#                                  kern.size=3,
#                                  kern.shape="disc",
#                                  ws.tolerance=1,
#                                  ws.radius=1,
#                                  displayprocessing=FALSE,
#                                  ...) # for the single channel images/for one channel of multi-channel images
# {
#   #   cat("do this - processing the single channel")
#   out <- vector(length(x),mode="list")
#   class(out) <- c("FrameList",class(out))
#   
#   for(i in 1:length(x))
#   {
#     rawimg <- x[[i]]$image
#     #     colorMode(rawimg) <- Grayscale
#     
#     flo = makeBrush(brush.size, brush.shape, step=FALSE)^2
#     flo <- flo/sum(flo)
#     
#     thresh_img <- thresh(filter2(rawimg,flo),w=at.wwidth,h=at.wheight,offset=at.offset)
#     
#     # if needed with a step of smoothing & co (operations of opening,...)
#     kern <- makeBrush(size=kern.size,shape=kern.shape)
#     
#     distmap_thre <- distmap(thresh_img)
#     watershed_thre <- watershed(distmap_thre,tolerance=ws.tolerance,ext=ws.radius) 
#     
#     out[[i]]$image <- watershed_thre
#     out[[i]]$channel <- x[[i]]$channel
#     out[[i]]$location <- x[[i]]$location  # or maybe call it location raw? 
#   }
#   return(out)
# }






#' Extracts particles from the images of a \code{FrameList} object. 
#' 
#'  
#' @param framelistRaw A \code{FrameList} object with the raw images (mandatory)
#' @param framelistPreprocessed A \code{FrameList} object with preprocessed images (optional, if not provided gets produced with standard default parameters)
#' @param channel Character string. The channel to perform the operations on. Can be \code{red}, \code{green} or \code{blue}
#' 
#' @return A \code{ParticleList} object, containing all detected particles for each frame
#' 
#' @examples
#' data("MesenteriumSubset")
#' platelets.framelist <- channels(MesenteriumSubset)$red
#' platelets.preprocessed <- preprocess(platelets.framelist)
#' particles.platelets <- particles(platelets.framelist, platelets.preprocessed)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
particles <- function(framelistRaw,
                      framelistPreprocessed=NULL,
                      channel=""  # if we provide the channelsFrameList as input 
)
{
  if(!is(framelistRaw,"FrameList") && !is(framelistRaw,"ChannelsFrameList"))
  {
    stop("You need to provide at least a FrameList/channelsFrameList object as input!")
  }
  
  
  if(is.null(framelistPreprocessed))
  {
    cat("You did not provide a preprocessed FrameList alongside with the raw set of frames.\n")
    cat("Don't worry, the raw FrameList object will be first preprocessed with a set of default parameter.\n")
    cat("You can always change them afterwards if they do not fit to your scenario.\n")
    if(is(framelistRaw,"ChannelsFrameList"))
    {
      #       framelistRaw <- framelistRaw[[channel]]
      framelistPreprocessed <- preprocess.ChannelsFrameList(framelistRaw[[channel]])
    } else {
      framelistPreprocessed <- preprocess.FrameList(framelistRaw)
    }
  }
  
  # check that both input framelists have same length
  if(length(framelistRaw) != length(framelistPreprocessed) )
  {
    stop("FrameList objects have different lengths!")
  } else {
    cat("Computing features...\n")
  }
  
  # returns a particle list - not linked yet
  
  out <- vector(length(framelistRaw),mode="list")
  class(out) <- c("ParticleList",class(out))
  
  for(i in 1:length(framelistRaw))
  {
    segmImg <- framelistPreprocessed[[i]]$image
    rawImg <- framelistRaw[[i]]$image
    
    imgFeatures <- as.data.frame(computeFeatures(segmImg,rawImg,xname="cell"))
    imgFeatures$shapeFactor <- (imgFeatures$cell.0.s.perimeter)^2 / (4*pi*imgFeatures$cell.0.s.area)
    
    out[[i]]$particles <- imgFeatures
    out[[i]]$imgSource <- framelistPreprocessed[[i]]$location
    out[[i]]$channel <- framelistPreprocessed[[i]]$channel
  }
  cat("Done!\n")
  return(out)
}

particles3 <- function(raw.frames,
                       binary.frames=NULL,
                       channel=NULL  
)
{
  # if still storing all channels, select one
  if(raw.frames@channel == "all" && is.null(channel))
    stop("Please select one channel to work on. Choose one among 'red','green' and 'blue'")
  if(raw.frames@channel == "all" && !is.null(channel))
    raw.frames <- channel.Frames(raw.frames,mode = channel)
  if ( missing(channel) ) # stop("Please provide a channel name to process")
    channel <- raw.frames@channel
  if(raw.frames@channel != "all" && channel != raw.frames@channel)
    stop("You are selecting to work on a channel not stored in your current Frames object")
  if(!is.null(binary.frames) && (raw.frames@channel != binary.frames@channel))
    stop("Your raw.frames and binary.frames objects store data related to different channels")
  
  
  if(missing(binary.frames)){
    cat("You did not provide a preprocessed FrameList alongside with the raw set of frames.\n")
    cat("Don't worry, the raw FrameList object will be first preprocessed with a set of default parameter.\n")
    cat("You can always change them afterwards if they do not fit to your scenario.\n")
    binary.frames = preprocess.Frames(raw.frames)
  }
  if ( length.Frames(raw.frames) != length.Frames(binary.frames) ) {
    stop("Raw and preprocessed Frames objects have different number of frames!")
  } else {
    cat("Computing features...\n")
  }
  
  # returns a particle list - not linked yet
  out <- ParticleSet(channel = binary.frames@channel)
  
  for(i in 1:length.Frames(raw.frames))  {
    segmImg <- getFrame(binary.frames, i)
    rawImg <- getFrame(raw.frames, i)
    
    imgFeatures <- as.data.frame(computeFeatures(segmImg,rawImg,xname="cell"))
    imgFeatures$shapeFactor <- (imgFeatures$cell.0.s.perimeter)^2 / (4*pi*imgFeatures$cell.0.s.area)
    
    # with the locations now saved as names
    out[[dimnames(binary.frames)[[3]][i]]] <- imgFeatures
  }
  cat("Done!\n")
  return(out)
}

# particles2 <- function(framelistRaw,
#                       framelistPreprocessed=NULL,
#                       channel=""  # if we provide the channelsFrameList as input 
# )
# {
#   if(!is(framelistRaw,"FrameList") && !is(framelistRaw,"ChannelsFrameList"))
#   {
#     stop("You need to provide at least a FrameList/channelsFrameList object as input!")
#   }
#   if(is.null(framelistPreprocessed))
#   {
#     cat("You did not provide a preprocessed FrameList alongside with the raw set of frames.\n")
#     cat("Don't worry, the raw FrameList object will be first preprocessed with a set of default parameter.\n")
#     cat("You can always change them afterwards if they do not fit to your scenario.\n")
#     if(is(framelistRaw,"ChannelsFrameList"))
#     {
#       #       framelistRaw <- framelistRaw[[channel]]
#       framelistPreprocessed <- preprocess.ChannelsFrameList(framelistRaw[[channel]])
#     } else {
#       framelistPreprocessed <- preprocess.FrameList(framelistRaw)
#     }
#   }
#   
#   # check that both input framelists have same length
#   if(length(framelistRaw) != length(framelistPreprocessed) )
#   {
#     stop("FrameList objects have different lengths!")
#   } else {
#     cat("Computing features...\n")
#   }
#   
#   # returns a particle list - not linked yet
#   
#   
#   
#   
#   
#   ## here i should simplify the list
#   out <- vector(length(framelistRaw),mode="list")
#   class(out) <- c("ParticleList",class(out))
#   
#   for(i in 1:length(framelistRaw))
#   {
#     segmImg <- framelistPreprocessed[[i]]$image
#     rawImg <- framelistRaw[[i]]$image
#     
#     imgFeatures <- as.data.frame(computeFeatures(segmImg,rawImg,xname="cell"))
#     imgFeatures$shapeFactor <- (imgFeatures$cell.0.s.perimeter)^2 / (4*pi*imgFeatures$cell.0.s.area)
#     
#     out[[i]]$particles <- imgFeatures
#     out[[i]]$imgSource <- framelistPreprocessed[[i]]$location
#     out[[i]]$channel <- framelistPreprocessed[[i]]$channel
#   }
#   cat("Done!\n")
#   return(out)
# }





#' Performs filtering on a \code{ParticleList} object
#' 
#' According to parameters of interests, such as size, eccentricity/shape, filters out the particles that do not 
#' satisfy the indicated requirements
#' 
#' @param particlelist A \code{ParticleList} object. A \code{LinkedParticleList} object can also be provided as input, yet the returned object will be a \code{ParticleList} object that needs to be linked again 
#' @param min.area Size in pixels of the minimum area needed to detect the object as a potential particle of interest
#' @param max.area Size in pixels of the maximum area allowed to detect the object as a potential particle of interest
#'  
#' @return A \code{ParticleList} object
#' 
#' @examples
#' data("candidate.platelets")
#' selected.platelets <- select.particles(candidate.platelets, min.area = 5)
#' selected.platelets
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
select.particles <- function(particleset,
                             min.area = 1,
                             max.area = 1000 #, # and others of interest, for example
                             #shapeThreshold = 0.5,
                             #eccentricityThreshold = 0.7 # currently not so efficient with so small particles available in the images!!
)
{
  # returns a particle list - not linked yet
  
  if(is(particleset,"LinkedParticleSet"))
  {
    cat("Warning, you are filtering particles that were previously linked by tracking them - reperform the linking afterwards.\n")
    cat("Filtering the particles...\n")
    out <- ParticleSet(x = particleset@.Data, channel = particleset@channel)
    # i have to change the class here
  } else {
    if(is(particleset,"ParticleSet")) 
    {
      out <- particleset
      cat("Filtering the particles...\n")
    } else {
      stop("You need to provide a ParticleSet object as input for select.particles!\n")
    }
  }
  
  
  for(i in 1:length(particleset))
  {
    candidateParticles <- particleset[[i]]
    
    nobjects <- nrow(candidateParticles)
    candidateParticles$shapeFactor <- (candidateParticles$cell.0.s.perimeter)^2 / (4*pi*candidateParticles$cell.0.s.area)
    
    notBecauseOfArea <- c()
    notBecauseOfEccen <- c()
    notBecauseOfShape <- c()
    
    notBecauseOfArea <- which( (candidateParticles$cell.0.s.area < min.area) | (candidateParticles$cell.0.s.area > max.area) )
    #     notBecauseOfEccen <- which(candidateParticles$cell.0.m.eccentricity > eccenThreshold)
    
    leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
    if(length(leftOut)!=0)#     if(!is.null(leftOut))
    {
      filteredParticles <- candidateParticles[-leftOut,]
    } else {
      filteredParticles <- candidateParticles
    }
    #       cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
    
    
    out[[i]] <- filteredParticles
  }
  return(out)
}




#################### newfile ######################


#' Calculate Otsu's threshold
#' 
#' Determines the value for the threshold grey level according to the Otsu method 
#'  
#' @param input_image An \code{Image} object
#' @param nr_bit Number of bits to use for discretizing the levels
#' 
#' @return A numeric value
#'  
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# otsuThreshold <- function (input_image,nr_bit=16)  # use all channels?
# {
#   # image is read through readImage from EBImage
#   # returns the level where the variance between classes is maximized
#   ##STEPS NOT NEEDED!!!!    
#   # transform into grey levels
#   #  greyImg <- EBImage::channel(input_image,"grey")
#   # the matrix gets stretched into a vec
#   #  greyVec <- as.vector(greyImg)
#   
#   
#   ## in order to treat properly the different channels, "step out like this"
#   
#   greyVec <- input_image
#   totPixels <- length(greyVec)
#   # and the values are rescaled
#   #  greyLevels <- greyVec * 255
#   #  greyLevels <- round(greyLevels)
#   
#   #  tableGrey <- tabulate(greyLevels)
#   # tableGrey <- tableGrey + 0.0001
#   
#   #  n_i <- tableGrey
#   #  p_i <- n_i/totPixels
#   # careful, sum is not exactly 1...
#   
#   ## coding in 16 bit, slightly better
#   greyLevels <- greyVec * (2^nr_bit -1) 
#   
#   #### modified!!
#   greyLevels <- as.vector(greyLevels)
#   
#   
#   greyLevels <- round(greyLevels)
#   
#   tableGrey <- tabulate(greyLevels)
#   tableGrey <- tableGrey + 0.000001
#   
#   n_i <- tableGrey
#   p_i <- n_i/totPixels
#   # careful, sum is not exactly 1...
#   
#   ## defining functions for means and probabilities - what goes afterwards in the otsu formula to maximize
#   ##
#   mean_T <- function(p, L) #L=length(tableGrey) )
#   {
#     ind <- 1:L
#     s <- ind * p[1:L]
#     z <- sum(s)
#     return(z)
#   }
#   ##
#   mean_k <- function(p, k, L)
#   {
#     ind <- 1:k
#     s <- ind * p[1:k]
#     z <- sum(s)
#     return(z)
#   }
#   ##
#   w_k <- function(p, k)
#   {
#     s <- sum(p[1:k])
#     return(s)
#   }
#   
#   ## define the function to optimize
#   sigma_B_k <- function(p, L, k)
#   {
#     ( mean_T(p,L) * w_k(p,k) - mean_k(p,k) )^2 / ( w_k(p,k) * (1 - w_k(p,k)) )
#   }
#   
#   ## actually do the optimization (max required)
#   o <- optimize(sigma_B_k, c(1,length(tableGrey)), maximum=TRUE, L=length(tableGrey), p=p_i )
#   otsu_threshold <- o$maximum/(2^nr_bit-1) 
#   
#   cat("Calculated value with Otsu thresholding method...")
#   cat(otsu_threshold)
#   cat("\n")
#   
#   return(otsu_threshold)
#   
# }
# 
# 
# ################
# 
# #' Calculate Kittler-Illinger's threshold
# #' 
# #' Determines the value for the threshold grey level according to the Kittler-Illinger method 
# #'   
# #' @param input_image An \code{Image} object
# #' @param nr_bit Number of bits to use for discretizing the levels
# #' 
# #' @return A numeric value
# #' 
# #' 
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# kittlerillingerThreshold <- function (input_image,nr_bit=8)  # use all channels?
# {
#   greyVec <- input_image
#   totPixels <- length(greyVec)
#   ## coding in 16 bit, slightly better
#   greyLevels <- greyVec * (2^nr_bit -1) # parametrizable? nrBit <- 16
#   # greyLevels <- greyVec * (2^8 -1) # parametrizable? nrBit <- 16
#   #### modified!!
#   greyLevels <- as.vector(greyLevels)
#   greyLevels <- round(greyLevels)
#   
#   tableGrey <- tabulate(greyLevels)
#   tableGrey <- tableGrey + 0.000001
#   
#   
#   n_i <- tableGrey
#   p_i <- n_i/totPixels
#   # careful, sum is not exactly 1...
#   
#   
#   P_t <- function(p, k)
#   {
#     s <- sum(p[0:k])
#     return(s)
#   }
#   
#   
#   ##
#   mean_f <- function(p, k, L)
#   {
#     ind <- 1:k
#     s <- ind * p[1:k]
#     z <- sum(s)
#     return(z)
#   }
#   
#   mean_b <- function(p, k, L)
#   {
#     ind <- (k+1):L
#     s <- ind * p[(k+1):L]
#     z <- sum(s)
#     return(z)
#   }
#   
#   sigma_f_t <- function(p, k, L)
#   {
#     s <- 0
#     mu <- mean_f(p,k,L)
#     for (i in 1:k)
#     {
#       s <- s + ( (i - mu)^2 * p[i] )
#     }
#     return(sqrt(s))
#   }
#   
#   sigma_b_t <- function(p, k, L)
#   {
#     s <- 0
#     mu <- mean_f(p,k,L)
#     for (i in (k+1):L)
#     {
#       s <- s + ( (i - mu)^2 * p[i] )
#     }
#     return(sqrt(s))
#   }
#   
#   
#   ## define the function to optimize
#   kittIll <- function(p, L, k)
#   {
#     P_t(p,k)*log(sigma_f_t(p,k,L)) + (1-P_t(p,k))*log(sigma_b_t(p,k,L)) - P_t(p,k)*log(P_t(p,k)) - (1-P_t(p,k))*log((1-P_t(p,k)))
#   }
#   
#   ## actually do the optimization (max required)
#   o <- optimize(kittIll, c(1,length(tableGrey)), maximum=FALSE, L=length(tableGrey), p=p_i )
#   kittlerillinger_threshold <- o$minimum/((2^nr_bit)-1) 
#   
#   cat("Calculated value with Kittler-Illinger thresholding method...")
#   cat(kittlerillinger_threshold)
#   cat("\n")
#   
#   return(kittlerillinger_threshold)
#   
# }


#################### newfile ######################

#' Links a \code{ParticleList} object
#' 
#' Performs linking of the particles by tracking them through the frames
#'  
#' 
#' @param particlelist A \code{ParticleList} object
#' @param L Maximum number of pixels an object can move in two consecutive frames
#' @param R Linkrange, i.e. the number of consecutive frames to search for potential candidate links
#' @param epsilon1 A numeric value, to be used in the formula. Jitter for allowing angular displacements
#' @param epsilon2 A numeric value, to be used in the formula. Jitter for allowing spatial displacements
#' @param lambda1 A numeric value. Multiplicative factor for the penalty function
#' @param lambda2 A numeric value. Multiplicative factor applied to the angular displacement
#' @param penaltyFunction A function structured in such a way to be applied as penalty function in the linking
#' @param nframes Numeric value. Number of frames where the linking should be performed 
#' @param verboseOutput Logical, whether the output should report additional intermediate steps. For debugging use mainly
#' @param prog Logical, whether the a progress bar should be shown during the tracking phase
#' @param include.intensity Logical, whether to include also intensity change of the particles in the cost function calculation
#' @param include.area Logical, whether to include also area change of the particles in the cost function calculation
#' 
#' @references I F Sbalzarini and P Koumoutsakos."Feature point tracking and trajectory analysis for video imaging in cell biology."
#' In: Journal of structural biology 151.2 (Aug. 2005), pp. 182-95. ISSN: 1047-8477. DOI: 10.1016/j.jsb.2005.06.002.
#' URL: http://www.ncbi.nlm.nih.gov/pubmed/16043363
#' 
#' @return A \code{LinkedParticleList} object
#' 
#' @examples
#' data("candidate.platelets")
#' tracked.platelets <- link.particles(candidate.platelets, L= 40)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
link.particles <- function(particleset,
                           L,
                           R=2,
                           epsilon1=0.1,
                           epsilon2=2,
                           lambda1=1,
                           lambda2=1,
                           penaltyFunction=penaltyFunctionGenerator(),
                           verboseOutput=FALSE,
                           prog=FALSE,
                           include.intensity=TRUE,
                           include.area=TRUE)
{
  out <- initialize.LinkedParticleSet(particleset)
  if (prog) pb <- txtProgressBar(min = 0, max = 100, style = 3)
  # preliminary step to set the particles next arrays (or matrix in our case) according to the linkrange
  # done now in case linkrange can get modified during the runs or across different runs
  if(verboseOutput) cat("Starting up...\n\n")
  frames_number <- length(out)
  linkrange <- R
  for (fr in 1:frames_number)
  {
    out@tracking[[fr]]$nxt <- matrix(0,nrow=nrow(out@.Data[[fr]]),ncol=R)
  }
  
  curr_linkrange <- R # as it can be updated afterwards
  displacement <- L
  # we need at least two frames
  if(frames_number < 2) 
  {
    stop("At least",R+1,"frames are needed! - Interrupting the tracking...\n")
  }
  
  # if the linkrange is too big, set it to the right value
  if(frames_number < (curr_linkrange+1))
  {
    curr_linkrange <- frames_number - 1
  }
  
  for(m in 1:(frames_number - 1)) 
  {
    if(verboseOutput) cat("---------- Linking frame",m,"of",frames_number,"...........\n")
    nop <- nrow(out@.Data[[m]])
    out@tracking[[m]]$special[] <- FALSE
    out@tracking[[m]]$nxt[,] <- -1
    
    # AND HERE BEGINS THE "TRUE" CYCLE THROUGHOUT NEXT FRAMES
    iterate <- 0
    for(n in 1:curr_linkrange)
    {
      iterate <- 0
      max_cost <- n*displacement*displacement    
      nop_next <- nrow(out@.Data[[m+n]])     
      ## cost matrix
      # setup the cost matrix
      C <- n*displacement*displacement*matrix(1,nop+1,nop_next+1)
      ## PLUS VARIATION
      # quadratic distance between p_i and q_j
      xrep <- repmat(t(out@.Data[[m+n]][,1]),nop,1) # function repmat replicates the matlab behaviour
      yrep <- repmat(t(out@.Data[[m+n]][,2]),nop,1)
      xdiff <- xrep - repmat(out@.Data[[m]][,1],1,nop_next)
      ydiff <- yrep - repmat(out@.Data[[m]][,2],1,nop_next)
      deltaSquared <- xdiff^2 + ydiff^2
      alpha <- atan2(ydiff,xdiff)
      
      deltaSquared <- (deltaSquared - epsilon2) * ((deltaSquared - epsilon2) > 0)
      # this allows to have a mini jitter on the position of the particle, that can now go slightly backwards and or move orthogonally to the flow
      
      ## similarly, include the part where the area/intensity is considered
      areaVariation <- 0
      # using the ... 6th column, corresponding to the area
      areaDiff <- repmat(t(out@.Data[[m+n]][,"cell.0.s.area"]),nop,1) - repmat(out@.Data[[m]][,"cell.0.s.area"],1,nop_next)
      areaVariation <- areaDiff^2
      
      intensityVariation <- 0
      # using the ... corresponding column, corresponding to the mean intensity
      intDiff <- repmat(t(out@.Data[[m+n]][,"cell.a.b.mean"]),nop,1) - repmat(out@.Data[[m]][,"cell.a.b.mean"],1,nop_next)
      intensityVariation <- intDiff^2
      
      
      #       distFunction <- deltaSquared + ifelse(include.area,areaVariation,0) + ifelse(include.intensity,intensityVariation,0)
      distFunction <- deltaSquared
      if(include.intensity) distFunction <- distFunction + intensityVariation
      if(include.area) distFunction <- distFunction + areaVariation
      
      
      newCost <- penaltyFunction(alpha,distFunction)
      
      #       newCost <- deltaSquared
      # cost function for link p_i, q_j
      # for everything apart from the dummies, already set to rL^2...
      C[1:nop,1:nop_next] <- newCost # + squared moments and so on
      C[nop+1,nop_next+1] <- 0 # dummy to dummy      
      C[C>max_cost] <- Inf
      
      ## association matrix
      # empty association matrix  
      A <- matrix(0,nrow=nop+1,ncol=nop_next+1)
      # initialize link matrix A --- oldstyle, but working ;) and probably even more optimized than the for for for loops
      for(i in 1:nop)
      {
        # sort costs of real particles
        srtcst <- sort(C[i,])
        srtidx <- order(C[i,])
        # append index of dummy
        iidx <- 1
        dumidx <- which(srtidx==nop_next+1)
        # search for available particle of smallest cost or dummy
        while(sum(A[,srtidx[iidx]])!=0 && (iidx < dumidx))
        {
          iidx <- iidx +1      
        }
        A[i,srtidx[iidx]] <- 1
      }   
      # set dummy particle for columns with no entry
      s <- colSums(A) 
      A[nop+1,which(s<1)] <- 1
      # dummy corresponds to dummy
      A[nop+1,nop_next+1] <- 1
      # consistency checks
      s1 <- colSums(A[,1:nop_next])
      s2 <- rowSums(A[1:nop,]) 
      # optimize the relation matrix
      finished <- 0
      mincost <- c()
      while(!finished)
      {
        iterate <- iterate + 1
        if(verboseOutput) cat("Frame:",m,"\t---\tLink_range:",n,"\t---\tIteration",iterate,".....")
        # non-set links of finite costs
        todo <- intersect(which(A[1:nop,1:nop_next]==0),which(C[1:nop,1:nop_next]<Inf) )
        Icand = ((todo-1) %% nop) + 1
        Jcand = floor((todo-1) / nop) + 1
        # determine the reduced cost Cred for each candidate insertion
        # initialize
        
        Cred <- rep(0,length(todo))
        Xcand <- rep(0,length(todo))
        Ycand <- rep(0,length(todo))
        # compute iteratively
        for(ic in 1:length(Icand))
        {
          Xcand[ic] <- which(A[Icand[ic],]==1)
          Ycand[ic] <- which(A[,Jcand[ic]]==1)
          Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        }
        
        # find minimum cost of corresponding action
        minc <- sort(Cred)[1]
        mini <- order(Cred)[1]
        mincost <- c(mincost,minc)
        
        # if minimum < 0, link addition is favorable
        if(!is.na(minc) && (minc < 0))
        {
          if(verboseOutput) cat("--> Performing change.\n")
          # add link and update dependencies to preserve the topology
          A[Icand[mini],Jcand[mini]] <- 1
          A[Ycand[mini],Jcand[mini]] <- 0
          A[Icand[mini],Xcand[mini]] <- 0
          A[Ycand[mini],Xcand[mini]] <- 1
          
        } else {
          # the best is already done and there is no room for improving the cost function
          finished <- TRUE
          if(verboseOutput) cat("--> Found optimal solution for the current cost function!\n")
        }
        
        # consistency checks--..
        s1 <- colSums(A[,1:nop_next])
        s2 <- rowSums(A[1:nop,])
      }
      
      # convert link matrix to representation in the list format
      links <- which(A[1:nop,]==1,arr.ind=TRUE)
      Ilinks <- links[,1]
      Jlinks <- links[,2]
      # if link is to dummy particle, set index to -1
      Jlinks[which(Jlinks==(nop_next+1))] <- -1
      
      # set links in the list object
      out@tracking[[m]]$nxt[Ilinks,n] <- Jlinks
    }
    # shrink curr_linkrange if needed at the end of the frames
    if(m==(frames_number-curr_linkrange) && curr_linkrange > 1)
    {
      curr_linkrange <- curr_linkrange - 1
    }
    if (prog)setTxtProgressBar(pb, (m / (frames_number - curr_linkrange + 1)*100))
  }
  # terminate all links at the list objects at the very last frame
  out@tracking[[frames_number]]$special[] <- FALSE
  out@tracking[[frames_number]]$nxt[,] <- -1 
  if (prog) {
    setTxtProgressBar(pb, 100)
    close(pb)
  }
#   class(out) <- c("LinkedParticleList",class(out))
  return(out)  
}





#' Generate a penalty function
#' 
#' A function to generate penalty functions to use while linking particles
#' 
#' @param epsilon1 A numeric value, to be used in the formula. Jitter for allowing angular displacements
#' @param epsilon2 A numeric value, to be used in the formula. Jitter for allowing spatial displacements
#' @param lambda1 A numeric value. Multiplicative factor for the penalty function
#' @param lambda2 A numeric value. Multiplicative factor applied to the angular displacement
#' 
#' @return A function object, to be used as penalty function
#' 
#' @examples
#' custom.function <- penaltyFunctionGenerator(epsilon1=0.1,epsilon2=6,lambda1=1.5,lambda2=0)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
penaltyFunctionGenerator <- function(epsilon1=0.1,
                                     epsilon2=2,
                                     lambda1=1,
                                     lambda2=1)
{
  # this function returns a function that is adopted afterwards in the tracking
  # it can be defined by default as we coded it, or the user can actually "invent" one of his taste 
  function(angle,distance) { 
    lambda1 * ( distance/ (1-lambda2*(angle/(pi+epsilon1))))
  }
}


#################### newfile ######################
#' Generate trajectories
#' 
#' Generates a \code{TrajectoryList} object from a (\code{Linked})\code{ParticleList}
#' 
#' @param particleset A (\code{Linked})\code{ParticleList} object
#' @param verbose Logical, currently not used - could be introduced for providing additional info on the trajectories
#' @param ... Arguments to be passed to methods
#' 
#' @return A \code{TrajectoryList} object
#' 
#' @examples
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
trajectories <- function(particleset,
                         verbose=FALSE,
                         ...)
{
  if(is(particleset,"LinkedParticleSet"))
  {
    cat("Generating trajectories...\n")
    linkedparticleset <- particleset
  } else {
    if(is(particleset,"ParticleSet"))
    {
      cat("Input ParticleList is not a LinkedParticleSet.\n")
      cat("Performing linking first with some set of default parameters - you might want to change them according to your scenario...\n")
      linkedparticleset <- link.particles(particleset,L=26,R=3,epsilon1=0,epsilon2=0,lambda1=1,lambda2=0)
      if(verbose) linkedparticleset
    }
  }
  
  out <- new("TrajectorySet", list(), channel = linkedparticleset@channel)
  
  ntraj <- 0
  linkrange <- ncol(linkedparticleset@tracking[[1]]$nxt)
  
  for(i in 1:length(linkedparticleset)) # looping over the frames
  {
    npart <- nrow(linkedparticleset@.Data[[i]])
    for(j in 1:npart)
    {
      if(linkedparticleset@tracking[[i]]$special[j]==FALSE) # if it did not already got used
      {
        linkedparticleset@tracking[[i]]$special[j] <- TRUE # set to true and proceed
        found <- -1
        for(n in 1:linkrange)
        {
          # if it is not a dummy particle, stop looking
          if(linkedparticleset@tracking[[i]]$nxt[j,n] != -1)
          {
            found <- n
            break
          }
        }
        
        # if this particle is not linked to any other, go to the next particle and do not add a trajectory
        if(found == -1)
        {
          next
        }
        # if this particle is linked to a real particle that was ALREADY linked, break the trajectory and start agin from the next
        # do not add a trajectory in this case
        if(linkedparticleset@tracking[[i+n]]$special[linkedparticleset@tracking[[i]]$nxt[j,n]])
        {
          next
        }
        # BUUUT, if this particle is linked to a real particle NOT already linked, then we DO have a trajectory
        ntraj <- ntraj+1
        traj <- c(linkedparticleset@.Data[[i]][j,1],linkedparticleset@.Data[[i]][j,2],ntraj,i,j)
        k <- i
        m <- j
        repeat
        {
          found <- -1
          for(n in 1:linkrange)
          {
            if(linkedparticleset@tracking[[k]]$nxt[m,n] != -1) # linked to a real particle -> continue building up the trajectory
            {
              if(linkedparticleset@tracking[[k+n]]$special[linkedparticleset@tracking[[k]]$nxt[m,n]] == FALSE) # which is not already linked
              {
                found <- n
                break
              } else {
                # it is linked to a real one, but already taken, then stop building the trajectory
                break
              }
            }
          }
          
          if(found == -1)
          {
            break
          }
          m <- linkedparticleset@tracking[[k]]$nxt[m,found]
          k <- k + found 
          # add to trajectory
          traj <- rbind(traj,c(linkedparticleset@.Data[[k]][m,1],linkedparticleset@.Data[[k]][m,2],ntraj,k,m))
          linkedparticleset@tracking[[k]]$special[m] <- TRUE
          
          if(m == -1) # to replicate the do while
          {
            break
          }
        }
        
        # generating output
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
  return(out)
}



#' Info on the dimensions of the FOV
#' 
#' Auxiliary function to return the dimensions of the field of interest
#'  
#' @param framelist A \code{FrameList} object
#' 
#' @return A list object, containing the extremes of the field of interest (x-y-z, where z is time)
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
axesInfo <- function(frames)
{
  imgDimensions <- dim(frames)
  out <- list(xlim=c(0,imgDimensions[1]),ylim=c(0,imgDimensions[2]),tlim=c(0,length.Frames(frames)))
  return(out)
}


#' 3D representation of a \code{TrajectoryList} object
#' 
#' Provides a visual representation of a \code{TrajectoryList} object
#' 
#' 
#' 
#' Based on the \code{rgl} library, the function extracts the region of interests from the dimensions of an image of the \code{FrameList} object,
#' and afterwards plots the x-y-time representation of the identified trajectories
#' 
#' @param x A \code{TrajectoryList} object
#' @param framelist A \code{FrameList} object, used here to identify the limits of the region of interest 
#' @param ... Arguments to be passed to methods
#' @param verbose Logical, whether to provide additional output on the command line
#' 
#' @method plot TrajectoryList
#' 
#' @examples
#' data("MesenteriumSubset")
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' \dontrun{
#' plot(platelets.trajectories,MesenteriumSubset)
#' }
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
plot.TrajectorySet <- function(trajectoryset,frames,verbose=FALSE,...)
{
  trajectoryDataFrame <- do.call(rbind.data.frame,lapply(trajectoryset,function(arg){arg$trajectory}))
  if(verbose) cat("Plotting",length(trajectoryset),"trajectories...\n")
  colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
  for (i in 1:max(trajectoryDataFrame$trajLabel))
  {
    singleTraj <- trajectoryDataFrame[which(trajectoryDataFrame$trajLabel==i),]
    rgl::plot3d(singleTraj$xCoord, singleTraj$yCoord, singleTraj$frame, col=colcols[singleTraj$trajLabel],type="l",lwd = 3,add=TRUE)
    
  }
  cubeLimits <- axesInfo(frames)
  #   
  decorate3d(xlim=cubeLimits$xlim,ylim=cubeLimits$ylim,zlim=cubeLimits$tlim,xlab="",ylab="",zlab="Frame Number",aspect=TRUE)
  bg3d("black") 
}



#' 2D projection of a \code{TrajectoryList} object
#' 
#' Provides a bird's eye view of a \code{TrajectoryList} object on a bidimensional space
#' 
#' Independent from the \code{rgl} library, the function extracts the region of interests from the dimensions of an image of the \code{FrameList} object,
#' and afterwards plots the x-y-time representation of the identified trajectories on a 2d plane. It is possible to subset the \code{TrajectoryList}
#' object with the IDs of the desired trajectories
#' 
#' @param x A \code{TrajectoryList} object
#' @param framelist A \code{FrameList} object, used here to identify the limits of the region of interest 
#' @param trajIDs A vector containing the ids of the desired trajectories
#' @param verbose Logical, whether to provide additional output on the command line
#' @param ... Arguments to be passed to methods
#' 
#' @examples
#' data("MesenteriumSubset")
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' plot2D.TrajectoryList(platelets.trajectories,MesenteriumSubset)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
plot2D.TrajectorySet <- function(trajectoryset,frames,trajIDs=NULL,verbose=FALSE,...)
{
  cubeLimits <- axesInfo(frames)
  xlim <- cubeLimits$xlim
  ylim <- cubeLimits$ylim
  
  colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
  if(is.null(trajIDs))
  {
    trajIDs <- 1:length(trajectoryset)
  } else {
    # check the IDs are correctly given
    allAvailableTrajectories <- sapply(trajectoryset,function(arg){unique(arg$trajectory$trajLabel)})
    if(!all(trajIDs %in% allAvailableTrajectories))
      stop("You are supplying IDs of trajectories which are not included in the TrajectoryList object!")
  }
  if(verbose) cat("Plotting",length(trajIDs),"trajectories (total available in the TrajectoryList:", length(trajectoryset), ")...\n")
  
  #   trajectoryDataFrame <- do.call(rbind.data.frame,lapply(trajectoryset,function(arg){arg$trajectory}))
  t <- trajIDs[1]
  plot(yCoord~xCoord, data = trajectoryset[[t]]$trajectory,
       xlim=xlim, ylim=ylim, xlab = "Pixel Coordinates - x axis",ylab="Pixel Coordinates - y axis",
       col=colcols[t],type = "l", lty = 1,lwd = 3,
       main= "Overview of the identified trajectories")
  #   text((yCoord + 2)~xCoord, data = trajectoryset[[t]]$trajectory, labels=t,
  #      col=colcols[t])
  # plotting just the label for the first point
  text((yCoord[1] + 4)~xCoord[1], data = trajectoryset[[t]]$trajectory, labels=t,
       col=colcols[t])
  for (t in trajIDs[-1])
  {
    lines(yCoord~xCoord, data = trajectoryset[[t]]$trajectory,
          col=colcols[t],type = "l", lty = 1,lwd = 3)
    text((yCoord[1] + 4)~xCoord[1], data = trajectoryset[[t]]$trajectory, labels=t,
         col=colcols[t])
  }
}




#' Add object contours to a \code{FrameList} object
#'  
#' Creates a \code{FrameList} object containing raw information, combined with the segmented images and the relative trajectory under analysis
#' 
#' If a \code{TrajectoryList} is provided and mode is set to \code{trajectories}, returns a \code{FrameList} with all trajectories included in the IDs 
#' vector painted accordingly.
#' If the mode is set to \code{particles}, it will just plot the particles (all) on all frames.
#' If no \code{trajectoryList} is provided, it will be computed with default parameters.
#' If no \code{binary.frames} is provided, it will be computed also with default parameters
#' 
#' @param raw.frames A \code{FrameList} object with raw images
#' @param binary.frames A \code{FrameList} object with preprocessed frames
#' @param trajectories A \code{TrajectoryList} object
#' @param trajIDs Numeric vector, the ID(s) of the trajectory.
#' @param mode A character string, can assume the values \code{particles} or \code{trajectories}. Defaults to \code{particles}
#' @param col A vector of color strings
#' @param channel A character string, to select which channel to process, if a \code{ChannelsFrameList} is supplied or if the \code{FrameList} in \code{raw.frames} has more than one channel
#' 
#' @return A new \code{FrameList} object with contours of the objects added
#' 
#' @examples
#' data("MesenteriumSubset")
#' \dontrun{
#' paintedTrajectories <- add.contours(raw.frames = MesenteriumSubset, mode = "trajectories",channel="red")
#' paintedParticles <- add.contours(raw.frames = MesenteriumSubset, mode = "particles",channel="red")
#' inspect.frames(paintedTrajectories)
#' inspect.frames(paintedParticles)
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
      stop("You are providing a TrajectoryList object, but no binary.frames FrameList!
           Please check whether you might have it in your workspace")
    }
  }
  
  
  # 
  if(mode=="trajectories") # additional checks on the trajectoryset object provided 
  {
    # if no trajectorylist is provided, compute it
    if(is.null(trajectoryset))
    {
      trajectoryset <- trajectories(particles3(raw.frames,binary.frames,channel=channel))
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
    
    out <- addTrajectories(raw.frames,binary.frames,trajectoryset,trajIDs)
    
  } else if(mode=="particles") # there is actually no need for the trajectorylist/trajId, and the col can be just one value ;)
  {
    out <- addParticles(raw.frames,binary.frames,col)
    
  } else {
    stop("The mode parameter must assume one value of the following: 'particles' or 'trajectories'!")
  }
  
  return(out)
}





addTrajectories <- function(raw.frames,binary.frames,trajectoryset,trajIDs){
  tmpFL <- lapply(1:length.Frames(raw.frames),function(i) Image(getFrame(raw.frames,i,"render")))
  colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
  for(i in trajIDs)
  {
    currentTraj <- trajectoryset[[i]]$trajectory
    counter <- 1
    for(j in currentTraj$frame)
    {
      rawimg <- tmpFL[[j]]
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





#################### newfile ######################

#################### newfile ######################

#################### newfile ######################