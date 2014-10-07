## all show methods

#' Display conveniently a \code{Frames} object
#' 
#' @param object A \code{Frames} object
#' @param ... Arguments to be passed to methods
#'  
#' @method print Frames
#' @return This returns an invisible \code{NULL}.
#' @examples
#' data("MesenteriumSubset")
#' print(MesenteriumSubset)
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
setMethod("show",
          signature = "Frames",
          definition = function(object){
            callNextMethod(object)
            cat("\nChannel(s):", object@channel)
            invisible(NULL)
          })


#' Display conveniently a \code{ParticleSet} object
#'  
#' @param object A \code{ParticleSet} object
#' @param ... Arguments to be passed to methods
#' 
#' @method print ParticleSet
#' 
#' @examples
#' data("candidate.platelets")
#' print(candidate.platelets)
#' 
#' @return This returns an invisible \code{NULL}.
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
            cat("\nParticles identified on the",object@channel,"channel\n") 
            invisible(NULL)
          })
  

#' Display conveniently a \code{LinkedParticleSet} object
#'  
#' @param object A \code{LinkedParticleSet} object
#' @param ... Arguments to be passed to methods
#' 
#' @method print LinkedParticleSet
#' 
#' @examples
#' data("candidate.platelets")
#' linked.platelets <- link.particles(candidate.platelets,L=26,R=3,epsilon1=0,
#' epsilon2=0,lambda1=1,lambda2=0,penaltyFunction=penaltyFunctionGenerator(),
#' include.area=FALSE)
#' print(linked.platelets)
#' 
#' @return This returns an invisible \code{NULL}.
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
            invisible(NULL)
          })



#' Display conveniently a \code{TrajectorySet} object
#'  
#' @param object A \code{TrajectorySet} object
#' @param ... Arguments to be passed to methods
#' 
#' @method print TrajectorySet
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' print(platelets.trajectories)
#' 
#' @return This returns an invisible \code{NULL}.
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
            
            cat("\nTrajectories are related to particles identified on the",object@channel,"channel\n")        
            invisible(NULL)
          })



#' Displaying conveniently a \code{KinematicsFeatures} object
#'  
#' @param object A \code{KinematicsFeatures} object
#' @param ... Arguments to be passed to methods
#' 
#' @method print KinematicsFeatures
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' traj11features <- kinematics(platelets.trajectories,trajectoryIDs = 11)
#' print(traj11features)
#' 
#' @return This returns an invisible \code{NULL}.
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
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
            invisible(NULL)
          })


#' Display conveniently a \code{KinematicsFeatureSet} object
#'  
#' @param object A \code{KinematicsFeatureSet} object
#' @param ... Arguments to be passed to methods
#' 
#' 
#' @examples 
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' alltrajs.features <- kinematics(platelets.trajectories)
#' print(alltrajs.features)
#'
#' @return This returns an invisible \code{NULL}.
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
            invisible(NULL)
          })








