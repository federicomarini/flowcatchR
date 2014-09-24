## all classes

setClass("Frames",
         contains = "Image",
         slots = c(
           channel = "character"
           )
         )

setClass("ParticleSet",
         contains = "list",
         slots = c(
           channel = "character"
           )
         )

setClass("LinkedParticleSet",
         contains = "list",
         slots = c(
           channel = "character",
           tracking = "list"
          )
        )



setClass("TrajectorySet",
         contains = "list",
         slots = c(
           channel = "character"
           )
         )

setClass("KinematicsFeatures",
         slots = c(
           Data = "list"
           )
)

setClass("KinematicsFeaturesSet",
         slots = c(
           Data = "list"
           )
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





