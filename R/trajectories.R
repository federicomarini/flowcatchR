#' Generate trajectories
#' 
#' Generates a \code{TrajectorySet} object from a (\code{Linked})\code{ParticleSet}
#' 
#' @param particleset A (\code{Linked})\code{ParticleSet} object
#' @param verbose Logical, currently not used - could be introduced for providing additional info on the trajectories
#' @param ... Arguments to be passed to methods
#' 
#' @return A \code{TrajectorySet} object
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
      cat("Input ParticleSet is not a LinkedParticleSet.\n")
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
#' @param frames A \code{Frames} object
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


#' 3D representation of a \code{TrajectorySet} object
#' 
#' Provides a visual representation of a \code{TrajectorySet} object
#' 
#' 
#' 
#' Based on the \code{rgl} library, the function extracts the region of interests from the dimensions of an image of the \code{Frames} object,
#' and afterwards plots the x-y-time representation of the identified trajectories
#' 
#' @param x A \code{TrajectorySet} object
#' @param frames A \code{Frames} object, used here to identify the limits of the region of interest 
#' @param ... Arguments to be passed to methods
#' @param verbose Logical, whether to provide additional output on the command line
#' 
#' 
#' @examples
#' data("MesenteriumSubset")
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' \dontrun{
#' plot(platelets.trajectories,MesenteriumSubset)
#' }
#' @return \code{plot.TrajectorySet} returns an invisible \code{NULL}.
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
plot.TrajectorySet <- function(x,frames,verbose=FALSE,...)
{
  trajectoryDataFrame <- do.call(rbind.data.frame,lapply(x,function(arg){arg$trajectory}))
  if(verbose) cat("Plotting",length(x),"trajectories...\n")
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
  invisible(NULL)
}



#' 2D projection of a \code{TrajectorySet} object
#' 
#' Provides a bird's eye view of a \code{TrajectorySet} object on a bidimensional space
#' 
#' Independent from the \code{rgl} library, the function extracts the region of interests from the dimensions of an image of the \code{Frames} object,
#' and afterwards plots the x-y-time representation of the identified trajectories on a 2d plane. It is possible to subset the \code{TrajectorySet}
#' object with the IDs of the desired trajectories
#' 
#' @param trajectoryset A \code{TrajectorySet} object
#' @param frames A \code{Frames} object, used here to identify the limits of the region of interest 
#' @param trajIDs A vector containing the ids of the desired trajectories
#' @param addGrid Logical, add an additional grid to the 2-dimensional plot (visual aid for backtracking trajectory point locations) 
#' @param verbose Logical, whether to provide additional output on the command line
#' @param ... Arguments to be passed to methods
#' 
#' @examples
#' data("MesenteriumSubset")
#' data("candidate.platelets")
#' platelets.trajectories <- trajectories(candidate.platelets)
#' plot2D.TrajectorySet(platelets.trajectories,MesenteriumSubset)
#' 
#' @return \code{plot2D.TrajectorySet} returns an invisible \code{NULL}.
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
plot2D.TrajectorySet <- function(trajectoryset,frames,trajIDs=NULL,addGrid=FALSE,verbose=FALSE,...)
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
      stop("You are supplying IDs of trajectories which are not included in the TrajectorySet object!")
  }
  if(verbose) cat("Plotting",length(trajIDs),"trajectories (total available in the TrajectorySet:", length(trajectoryset), ")...\n")
  
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
  if(addGrid) grid(col="darkgrey")
  invisible(NULL)
}








