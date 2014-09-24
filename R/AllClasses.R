## all classes

setClass("Frames",
         contains = "Image",
         slots = c(
           channel = "character"
           )
         )

setClass("ParticleSet",
         slots = c(
           Data = "list",
           channel = "character"
           )
         )

setClass("LinkedParticleSet",
         slots = c(
           Data = "list",
           channel = "character"
           )
         )


setClass("TrajectorySet",
         slots = c(
           Data = "list"
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
Frames = function(x, channel) {
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
