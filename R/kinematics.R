#' Calculate a set of kinematics parameter from a \code{TrajectorySet} object, or a single parameter, or from a single trajectory (all possible combinations)
#' 
#' The computed set of parameters include \code{delta.x}, \code{delta.t} and \code{delta.v}
#' (displacements and instantaneous velocity), \code{totalTime}, \code{totalDistance},
#' \code{distStartToEnd}, \code{curvilinearVelocity}, \code{straightLineVelocity} and
#' \code{linearityForwardProgression}, Mean Squared Displacement, velocity
#' autocorrelation, and more. If a single trajectory is specified, the computation is
#' performed for that trajectory alone. If a parameter is specified, only that
#' parameter is reported, either for one or all trajectories
#' 
#' @param trajectoryset A \code{TrajectorySet} object
#' @param trajectoryIDs The ID of a single trajectory
#' @param acquisitionFrequency The frame rate of acquisition for the images, in
#'   milliseconds
#' @param scala The value of micro(?)meters to which each single pixel
#'   corresponds
#' @param feature Character string, the name of the feature to be computed
#'   
#' @return A \code{KinematicsFeaturesSet} object, or a \code{KinematicsFeatures} object,
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
          straightLineVelocity, linearityForwardProgression, trajMSD,
          velocityAutoCorr, instAngle, directChange or dirAutoCorr")

      return(invisible(NULL))
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
    
  } else { # it will be done on all trajectories of the trajectoryset
    tmp <- list()
    for(i in 1:length(trajectoryset))
    {
      kineOne <- extractKinematics.traj(trajectoryset,i,acquisitionFrequency=acquisitionFrequency,scala=scala) #, acquisitionFrequency=30, # in milliseconds, scala=50
      tmp[[length(tmp) + 1]] <- kineOne
    }
    # additional class attribute?
    
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



#' Calculate a set of kinematics parameters from a single trajectory
#' 
#' The computed set of parameters include \code{delta.x}, \code{delta.t} and \code{delta.v}
#' (displacements and instantaneous velocity), \code{totalTime}, \code{totalDistance},
#' \code{distStartToEnd}, \code{curvilinearVelocity}, \code{straightLineVelocity} and
#' \code{linearityForwardProgression}, Mean Squared Displacement, velocity
#' autocorrelation, and more
#' 
#' @param trajectoryset A \code{TrajectorySet} object
#' @param trajectoryID The ID of a single trajectory
#' @param acquisitionFrequency The frame rate of acquisition for the images, in
#'   milliseconds
#' @param scala The value of micro(?)meters to which each single pixel
#'   corresponds
#'   
#' @return A \code{KinematicsFeatures} object
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








