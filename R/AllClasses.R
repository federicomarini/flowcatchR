## all classes


#' Frames class
#' 
#' S4 class for storing information on multiple images belonging to the same time-lapse experiment.
#' It is designed as a subclass of the existing Image class from the EBImage package
#' 
#' @slot channel A character vector, can be 'red','green','blue' or 'all' (if in color mode)
#' 
#' @exportClass Frames
setClass("Frames",
         contains = "Image",
         slots = c(
           channel = "character"
           )
         )

#' ParticleSet class
#' 
#' S4 class for storing information on particles detected in distinct frames.
#' 
#' @slot .Data A list storing the information for the particles
#' @slot channel A character vector, can be 'red','green', or 'blue'. It refers to which channel the particles were detected
#' 
#' @exportClass ParticleSet
setClass("ParticleSet",
         contains = "list",
         slots = c(
           channel = "character"
           )
         )


#' LinkedParticleSet class
#' 
#' S4 class for storing information of particles after they have been tracked. It inherits the slots
#' from the \code{ParticleSet} class.
#' 
#' @slot tracking A list storing all necessary information for the tracking algorithm to work, and for providing the information
#' to the function to determine the trajectories
#'
#' @exportClass LinkedParticleSet
setClass("LinkedParticleSet",
         contains = "ParticleSet",
         slots = c(
           tracking = "list"
           )
        )

#' TrajectorySet class
#' 
#' S4 class for storing information on the trajectories identified, including whether there were gaps, the number of points, and more
#' 
#' @slot .Data A list storing the information for the particles
#' @slot channel A character vector, can be 'red','green', or 'blue'. It refers to which channel the particles were detected
#'
#' @exportClass TrajectorySet
setClass("TrajectorySet",
         contains = "list",
         slots = c(
           channel = "character"
           )
         )


#' KinematicsFeatures class
#' 
#' S4 class for storing information on all kinematics features identified for a single trajectory
#' 
#' @slot .Data A list storing the information for the kinematics features
#'
#' @exportClass KinematicsFeatures
setClass("KinematicsFeatures",
         contains = "list"
        )

#' KinematicsFeaturesSet
#' 
#' S4 class for storing information on all kinematics features identified for all trajectories. Single
#' \code{KinematicsFeatures} objects are the element of the main list
#' 
#' @slot .Data A list storing the information for the sets of kinematics features
#' 
#' @exportClass KinematicsFeaturesSet
setClass("KinematicsFeaturesSet",
         contains = "list"
        )


## and their constructors
Frames <- function(x, channel) {
  if(channel=="all")
    new("Frames", x, channel = channel)
  else
  {
    if(channel=="red"||channel=="green"||channel=="blue")
      new("Frames", channel(x, channel), channel = channel)
    else
      stop("You need to provide one of the following values for the channel: 'red', 'green', 'blue', or 'all'")
  }
}


ParticleSet <- function(x = list(), channel){
  new("ParticleSet",x,channel=channel)
}





