## all show methods

#' Display conveniently a \code{Frames} object
#' 
#' @param object A \code{Frames} object
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


#' Display conveniently a \code{ParticleList} object
#'  
#' @param object A \code{ParticleList} object
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
#' @param object A \code{LinkedParticleList} object
#' @param ... Arguments to be passed to methods
#' 
#' @method print LinkedParticleList
#' 
#' @examples
#' data("candidate.platelets")
#' linked.platelets <- link.particles(candidate.platelets,L=26,R=3,epsilon1=0,
#' epsilon2=0,lambda1=1,lambda2=0,penaltyFunction=penaltyFunctionGenerator(),
#' include.area=FALSE)
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



#' Display conveniently a \code{TrajectoryList} object
#'  
#' @param object A \code{TrajectoryList} object
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
#' @param object A \code{KinematicsFeatureSet} object
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
#' @param object A \code{KinematicsFeatureSetList} object
#' @param ... Arguments to be passed to methods
#' 
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' alltrajs.features <- kinematics(platelets.trajectories)
#' print(alltrajs.features)
#'
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
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








