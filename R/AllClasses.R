## all classes

#' @exportClass Frames
setClass("Frames",
         contains = "Image",
         slots = c(
           channel = "character"
           )
         )
#' @exportClass ParticleSet
setClass("ParticleSet",
         contains = "list",
         slots = c(
           channel = "character"
           )
         )

# setClass("LinkedParticleSet",
#          contains = "list",
#          slots = c(
#            channel = "character",
#            tracking = "list"
#           )
#         )


#' @exportClass LinkedParticleSet
setClass("LinkedParticleSet",
         contains = "ParticleSet",
         slots = c(
           tracking = "list"
           )
        )

#' @exportClass TrajectorySet
setClass("TrajectorySet",
         contains = "list",
         slots = c(
           channel = "character"
           )
         )
#' @exportClass KinematicsFeatures
setClass("KinematicsFeatures",
         contains = "list"
        )

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





