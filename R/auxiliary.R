

#' Compute the length of render frames in a \code{Frames} object
#' 
#' @param x A \code{Frames} object
#' 
#' @return An integer number
#' @examples
#' data("MesenteriumSubset")
#' length(MesenteriumSubset)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
length.Frames <- function(x)
{
  numberOfFrames(x,"render")
}





#' Channel extraction for \code{objects}
#' 
#' \code{channel}
#' 
#' @param frames A \code{Frames} object
#' @param mode A character value specifying the target mode for conversion.
#' 
#' @return A \code{Frames} object with just the infotmation on the selected channel
#' @examples
#' data("MesenteriumSubset")
#' channel.Frames(MesenteriumSubset,"red")
#' @export
channel.Frames <- function(frames,mode)
{
  y <- Frames(frames,channel=mode)
  return(y)
}



#' Explore the frames of a \code{Frames}
#' 
#' The first frames of a \code{Frames} are displayed in the browser, and are interactively navigable.
#' 
#' 
#' @param frames A \code{Frames} object
#' @param nframes The number of frames to display (default value: \code{NULL}, all are displayed )
#' @param display.method Method for displaying, can be either \code{raster} or \code{browser}. Defaults to \code{browser}, by opening a window in the browser
#' @param verbose Logical, whether to provide additional output on the command line alongside with the images themselves
#' 
#' @examples
#' data("MesenteriumSubset")
#' \dontrun{inspect.Frames(MesenteriumSubset)}
#' 
#' @return \code{inspect.Frames} returns an invisible \code{NULL}.
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
  invisible(NULL)
}




#' Constructor for a \code{Frames} object
#' 
#' This function is used to create a \code{Frames} object from a vector of image files (or a folder specifying the directory
#' containing them). 
#' The number of frames is also specified, as just a subset of the images can be used for this
#' 
#' @param image.files Vector of strings containing the locations where the (raw) images are to be found, or alternatively, the path to the folder
#' @param nframes Number of frames that will constitute the \code{Frames} object
#'
#' @return An object of the \code{Frames} class, which holds the info on a list of frames, specifying for each the following elements:
#' \item{image}{The \code{Image} object containing the image itself}
#' \item{location}{The complete path to the location of the original image}
#' 
#'
#' @examples
#' ## see vignette
#' \dontrun{fullData <- read.Frames(image.files = "/path/to/the/directory", nframes = 100)}
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
read.Frames <- function(image.files, # ../exportedMesenteriumSubset
                        nframes=NULL)
{
  if(all(!file.exists(image.files)))
    stop("File/folder not found")
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
  
  multiImg <- readImage(image.files,names = image.files)
  
  y <- Frames(multiImg,channel = "all")  
  
  cat("Created a Frames object of",nframes,"frames.\n")
  return(y)
}






#' Exports a \code{Frames} object
#' 
#' Writes the images contained in the \code{image} slot of the \code{Frames} object elements.
#' The images can be exported as single frames, or as a .gif image that is composed
#' by the single frames.
#' 
#' @param frames A \code{Frames} object
#' @param dir The path of the folder where the image should be written
#' @param nameStub The stub for the file name, that will be used as a prefix for the exported images
#' @param createGif Logical, whether to create or not an animated .gif file
#' @param removeAfterCreatingGif Logical, whether to remove the single exported .png images after creating the single .gif
#' 
#' @return Image files are written in the desired location
#' 
#' @examples
#' data("MesenteriumSubset")
#' \dontrun{export.Frames(MesenteriumSubset,nameStub="subset_export_",createGif=TRUE,removeAfterCreatingGif=FALSE)}
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
  imgNames <- file.path(dir, paste0(nameStub, "_frame_", 
                                     formatC(1:totframes,width=nchar(totframes), flag="0"), ".png"))
  writeImage(frames,imgNames)
  
  if(createGif)
  {
    # using imagemagick
    system(sprintf("convert -delay 40 %s %s.gif", paste(imgNames, collapse = ""), file.path(dir, nameStub)))
  }
  if(removeAfterCreatingGif && createGif)
  {
    file.remove(imgNames)
  }
  invisible()
}




#' Extracts subsets of frames from a \code{Frames} object
#' 
#' An input \code{Frames} object is subject to subsetting. This function is useful e.g. when the trajectory of interest 
#' is presenting gaps (i.e. does not actually include a frame)
#' 
#' @param frames A \code{Frames} object
#' @param framesToKeep A vector containing the indexes of the frames to keep in the selection
#' @param ... Arguments to be passed to methods
#' 
#' @return A \code{Frames} object, composed by the subset of frames of the input \code{Frames}
#' 
#' 
#' @examples
#' data("MesenteriumSubset")
#' select.Frames(MesenteriumSubset, framesToKeep = c(1:10, 14:20))
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
select.Frames <- function(frames,framesToKeep=1,...)
{
  # check if all framesToKeep are actually available in the Frames object
  totframes <- length.Frames(frames)
  if (max(framesToKeep) > totframes)
    stop("You are selecting frames that are not available in the original Frames object")
  
  
  y <- combine(lapply(framesToKeep,function(i) getFrame(frames,i,type = "render")))
  d = if ( colorMode(frames) == Color ) 4L else 3L
  dimnames(y)[[d]] <- dimnames(frames)[[d]][framesToKeep]
  #   y <- Frames(multiImg,channel=frames@channel)
  
  return(y)
}





#' Constructor for a \code{ParticleSet} object
#' 
#' This function is used to create a \code{ParticleSet} object from a vector/list of tab separated text files, each of one containing one line for each 
#' particle in the related frame, alongside with its coordinates and if available, the computed features
#' The number of frames is also specified, as just a subset of the particle lists can be used for this
#' 
#' @param particle.files Vector of strings containing the locations where the particle coordinates are to be found, or alternatively, the path to the folder
#' @param nframes Number of frames that will constitute the \code{ParticleSet} object
#'
#' @return An object of the \code{ParticleSet} class 
#' 
#' @examples
#' ## see vignette and export.particles
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
read.particles <- function(particle.files,
                           nframes=NULL)
{
  if(!file.exists(particle.files))
    stop("File/folder not found")
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
  
  importedChannel <- unlist(strsplit(readLines(particle.files[1],n=1),split = "|",fixed=TRUE))[3]
  out <- ParticleSet(channel = importedChannel)
  
  for (i in 1:nframes)
  {
    imgSource <- unlist(strsplit(readLines(particle.files[i],n=1),split = "|",fixed=TRUE))[2]
    out[[imgSource]] <- read.table(particle.files[i],sep="\t",comment.char = "#",header=TRUE,stringsAsFactors = FALSE)
  }
  cat("Created a ParticleSet object of",nframes,"frames.\n")
  return(out)
}




#' Exports a \code{ParticleSet} object
#' 
#' Writes the particles contained in the \code{particles} data frame slot of the \code{ParticleSet} object elements.
#' A track of the provenience of the particles is stored as a comment line above the header
#' 
#' @param particleset A \code{ParticleSet} object
#' @param dir The path of the folder where the particle sets should be written
#' @param nameStub The stub for the file name, that will be used as a prefix for the exported particle sets
#' 
#' @return Particle sets files are written in the desired location
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
  totframes <- length(particleset)
  particleNames <- file.path(dir, paste0(nameStub, "_frame_", formatC(1:totframes,
                                                                  width=nchar(totframes), flag="0"), ".tsv"))
  for (i in 1:totframes)
  {
    writeLines(paste0("#|",names(particleset)[i],"|",particleset@channel),particleNames[[i]])
    write.table(particleset@.Data[[i]],particleNames[[i]],append = TRUE,sep = "\t",quote = FALSE,col.names = TRUE,row.names= FALSE)
  }
  cat("Done exporting the .tsv files for the ParticleSet.\n
      You can ignore the warning messages as long as you remind the additional comment line added starting with '#'")
  invisible()
}



#' Performs filtering on a \code{ParticleSet} object
#' 
#' According to parameters of interests, such as size, eccentricity/shape, filters out the particles that do not 
#' satisfy the indicated requirements
#' 
#' @param particleset A \code{ParticleSet} object. A \code{LinkedParticleSet} object can also be provided as input, yet the returned object will be a \code{ParticleSet} object that needs to be linked again 
#' @param min.area Size in pixels of the minimum area needed to detect the object as a potential particle of interest
#' @param max.area Size in pixels of the maximum area allowed to detect the object as a potential particle of interest
#'  
#' @return A \code{ParticleSet} object
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
  # returns a particleset - not linked yet
  
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



