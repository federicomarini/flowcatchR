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

