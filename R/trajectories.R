# #' Generate trajectories
# #' 
# #' Generates a \code{TrajectoryList} object from a (\code{Linked})\code{ParticleList}
# #' 
# #' @param particlelist A (\code{Linked})\code{ParticleList} object
# #' @param provideExtraDetails Logical, currently not used - could be introduced for providing additional info on the trajectories
# #' @param ... Arguments to be passed to methods
# #' 
# #' @return A \code{TrajectoryList} object
# #' 
# #' @examples
# #' data("candidate.platelets")
# #' platelets.trajectories <- trajectories(candidate.platelets)
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# trajectories <- function(particlelist,
#                          provideExtraDetails=FALSE,A
#                          ...)
# {
#   if(is(particlelist,"LinkedParticleList"))
#   {
#     cat("Generating trajectories...\n")
#     linkedparticlelist <- particlelist
#   } else {
#     if(is(particlelist,"ParticleList"))
#     {
#       cat("Input ParticleList is not a LinkedParticleList.\n")
#       cat("Performing linking first with some set of default parameters - you might want to change them according to your scenario...\n")
#       linkedparticlelist <- link.particles(particlelist,L=26,R=3,epsilon1=0,epsilon2=0,lambda1=1,lambda2=0)
#       print(linkedparticlelist)
#     }
#   }
#   
#   out <- vector(1,mode="list")
#   class(out) <- c("TrajectoryList",class(out))
#   
#   ntraj <- 0
#   linkrange <- ncol(linkedparticlelist[[1]]$nxt)
#   
#   for(i in 1:length(linkedparticlelist)) # looping over the frames
#   {
#     npart <- nrow(linkedparticlelist[[i]]$particles)
#     for(j in 1:npart)
#     {
#       if(linkedparticlelist[[i]]$special[j]==FALSE) # if it did not already got used
#       {
#         linkedparticlelist[[i]]$special[j] <- TRUE # set to true and proceed
#         found <- -1
#         for(n in 1:linkrange)
#         {
#           # if it is not a dummy particle, stop looking
#           if(linkedparticlelist[[i]]$nxt[j,n] != -1)
#           {
#             found <- n
#             break
#           }
#         }
#         
#         # if this particle is not linked to any other, go to the next particle and do not add a trajectory
#         if(found == -1)
#         {
#           next
#         }
#         # if this particle is linked to a real particle that was ALREADY linked, break the trajectory and start agin from the next
#         # do not add a trajectory in this case
#         if(linkedparticlelist[[i+n]]$special[linkedparticlelist[[i]]$nxt[j,n]])
#         {
#           next
#         }
#         # BUUUT, if this particle is linked to a real particle NOT already linked, then we DO have a trajectory
#         ntraj <- ntraj+1
#         traj <- c(linkedparticlelist[[i]]$particles[j,1],linkedparticlelist[[i]]$particles[j,2],ntraj,i,j)
#         k <- i
#         m <- j
#         repeat
#         {
#           found <- -1
#           for(n in 1:linkrange)
#           {
#             if(linkedparticlelist[[k]]$nxt[m,n] != -1) # linked to a real particle -> continue building up the trajectory
#             {
#               if(linkedparticlelist[[k+n]]$special[linkedparticlelist[[k]]$nxt[m,n]] == FALSE) # which is not already linked
#               {
#                 found <- n
#                 break
#               } else {
#                 # it is linked to a real one, but already taken, then stop building the trajectory
#                 break
#               }
#             }
#           }
#           
#           if(found == -1)
#           {
#             break
#           }
#           m <- linkedparticlelist[[k]]$nxt[m,found]
#           k <- k + found 
#           # add to trajectory
#           traj <- rbind(traj,c(linkedparticlelist[[k]]$particles[m,1],linkedparticlelist[[k]]$particles[m,2],ntraj,k,m))
#           linkedparticlelist[[k]]$special[m] <- TRUE
#           
#           if(m == -1) # to replicate the do while
#           {
#             break
#           }
#         }
#         
#         # generating output
#         out[[ntraj]] <- list()
#         
#         colnames(traj) <- c("xCoord","yCoord","trajLabel","frame","frameobjectID")
#         rownames(traj) <- paste0(ntraj,"_",seq(1:nrow(traj)))
#         traj <- as.data.frame(traj)
#         out[[ntraj]]$trajectory <- traj
#         out[[ntraj]]$npoints <- nrow(traj)
#         out[[ntraj]]$nframes <- traj$frame[nrow(traj)]-traj$frame[1] + 1
#         out[[ntraj]]$ngaps <- out[[ntraj]]$nframes - out[[ntraj]]$npoints
#         out[[ntraj]]$keep <- NA # initialized, then set to 0 or 1
#         out[[ntraj]]$ID <- ntraj
#       }
#     }
#   }
#   return(out)
# }
# 
# 
# 
# #' Info on the dimensions of the FOV
# #' 
# #' Auxiliary function to return the dimensions of the field of interest
# #'  
# #' @param framelist A \code{FrameList} object
# #' 
# #' @return A list object, containing the extremes of the field of interest (x-y-z, where z is time)
# #' 
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# axesInfo <- function(framelist)
# {
#   imgDimensions <- dim(framelist[[1]]$image)
#   out <- list(xlim=c(0,imgDimensions[1]),ylim=c(0,imgDimensions[2]),tlim=c(0,length(framelist)))
# }
# 
# 
# #' 3D representation of a \code{TrajectoryList} object
# #' 
# #' Provides a visual representation of a \code{TrajectoryList} object
# #' 
# #' 
# #' 
# #' Based on the \code{rgl} library, the function extracts the region of interests from the dimensions of an image of the \code{FrameList} object,
# #' and afterwards plots the x-y-time representation of the identified trajectories
# #' 
# #' @param x A \code{TrajectoryList} object
# #' @param framelist A \code{FrameList} object, used here to identify the limits of the region of interest 
# #' @param ... Arguments to be passed to methods
# #' @param verbose Logical, whether to provide additional output on the command line
# #' 
# #' @method plot TrajectoryList
# #' 
# #' @examples
# #' data("MesenteriumSubset")
# #' data("candidate.platelets")
# #' platelets.trajectories <- trajectories(candidate.platelets)
# #' \dontrun{
# #' plot(platelets.trajectories,MesenteriumSubset)
# #' }
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# plot.TrajectoryList <- function(x,framelist,verbose=FALSE,...)
# {
#   trajectoryDataFrame <- do.call(rbind.data.frame,lapply(x,function(arg){arg$trajectory}))
#   if(verbose) cat("Plotting",length(x),"trajectories...\n")
#   colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
#   for (i in 1:max(trajectoryDataFrame$trajLabel))
#   {
#     singleTraj <- trajectoryDataFrame[which(trajectoryDataFrame$trajLabel==i),]
#     rgl::plot3d(singleTraj$xCoord, singleTraj$yCoord, singleTraj$frame, col=colcols[singleTraj$trajLabel],type="l",lwd = 3,add=TRUE)
#     
#   }
#   cubeLimits <- axesInfo(framelist)
# #   
#   decorate3d(xlim=cubeLimits$xlim,ylim=cubeLimits$ylim,zlim=cubeLimits$tlim,xlab="",ylab="",zlab="Frame Number",aspect=TRUE)
#   bg3d("black") 
# }
# 
# 
# 
# #' 2D projection of a \code{TrajectoryList} object
# #' 
# #' Provides a bird's eye view of a \code{TrajectoryList} object on a bidimensional space
# #' 
# #' Independent from the \code{rgl} library, the function extracts the region of interests from the dimensions of an image of the \code{FrameList} object,
# #' and afterwards plots the x-y-time representation of the identified trajectories on a 2d plane. It is possible to subset the \code{TrajectoryList}
# #' object with the IDs of the desired trajectories
# #' 
# #' @param x A \code{TrajectoryList} object
# #' @param framelist A \code{FrameList} object, used here to identify the limits of the region of interest 
# #' @param trajIDs A vector containing the ids of the desired trajectories
# #' @param verbose Logical, whether to provide additional output on the command line
# #' @param ... Arguments to be passed to methods
# 
# #' @examples
# #' data("MesenteriumSubset")
# #' data("candidate.platelets")
# #' platelets.trajectories <- trajectories(candidate.platelets)
# #' plot2D.TrajectoryList(platelets.trajectories,MesenteriumSubset)
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# plot2D.TrajectoryList <- function(x,framelist,trajIDs=NULL,verbose=FALSE,...)
# {
#   cubeLimits <- axesInfo(framelist)
#   xlim <- cubeLimits$xlim
#   ylim <- cubeLimits$ylim
#   
#   colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
#   if(is.null(trajIDs))
#   {
#     trajIDs <- 1:length(x)
#   } else {
#     # check the IDs are correctly given
#     allAvailableTrajectories <- sapply(x,function(arg){unique(arg$trajectory$trajLabel)})
#     if(!all(trajIDs %in% allAvailableTrajectories))
#       stop("You are supplying IDs of trajectories which are not included in the TrajectoryList object!")
#   }
#   if(verbose) cat("Plotting",length(trajIDs),"trajectories (total available in the TrajectoryList:", length(x), ")...\n")
#   
# #   trajectoryDataFrame <- do.call(rbind.data.frame,lapply(x,function(arg){arg$trajectory}))
#   t <- trajIDs[1]
#   plot(yCoord~xCoord, data = x[[t]]$trajectory,
#      xlim=xlim, ylim=ylim, xlab = "Pixel Coordinates - x axis",ylab="Pixel Coordinates - y axis",
#      col=colcols[t],type = "l", lty = 1,lwd = 3,
#      main= "Overview of the identified trajectories")
# #   text((yCoord + 2)~xCoord, data = x[[t]]$trajectory, labels=t,
# #      col=colcols[t])
#   # plotting just the label for the first point
#   text((yCoord[1] + 4)~xCoord[1], data = x[[t]]$trajectory, labels=t,
#      col=colcols[t])
#   for (t in trajIDs[-1])
#   {
#     lines(yCoord~xCoord, data = x[[t]]$trajectory,
#           col=colcols[t],type = "l", lty = 1,lwd = 3)
#     text((yCoord[1] + 4)~xCoord[1], data = x[[t]]$trajectory, labels=t,
#          col=colcols[t])
#   }
# #   cubeLimits <- axesInfo(framelist)
# #   decorate3d(xlim=cubeLimits$xlim,ylim=cubeLimits$ylim,zlim=cubeLimits$tlim,xlab="",ylab="",zlab="",aspect=T)
#   
# }
# 
# 
# 
# 
# #' Add object contours to a \code{FrameList} object
# #'  
# #' Creates a \code{FrameList} object containing raw information, combined with the segmented images and the relative trajectory under analysis
# #' 
# #' If a \code{TrajectoryList} is provided and mode is set to \code{trajectories}, returns a \code{FrameList} with all trajectories included in the IDs 
# #' vector painted accordingly.
# #' If the mode is set to \code{particles}, it will just plot the particles (all) on all frames.
# #' If no \code{trajectoryList} is provided, it will be computed with default parameters.
# #' If no \code{binary.frames} is provided, it will be computed also with default parameters
# #' 
# #' @param raw.frames A \code{FrameList} object with raw images
# #' @param binary.frames A \code{FrameList} object with preprocessed frames
# #' @param trajectories A \code{TrajectoryList} object
# #' @param trajIds Numeric vector, the ID(s) of the trajectory.
# #' @param mode A character string, can assume the values \code{particles} or \code{trajectories}. Defaults to \code{particles}
# #' @param col A vector of color strings
# #' @param channel A character string, to select which channel to process, if a \code{ChannelsFrameList} is supplied or if the \code{FrameList} in \code{raw.frames} has more than one channel
# #' 
# #' @return A new \code{FrameList} object with contours of the objects added
# #' 
# #' @examples
# #' data("MesenteriumSubset")
# #' \dontrun{
# #' paintedTrajectories <- add.contours(raw.frames = MesenteriumSubset, mode = "trajectories",channel="red")
# #' paintedParticles <- add.contours(raw.frames = MesenteriumSubset, mode = "particles",channel="red")
# #' inspect.frames(paintedTrajectories)
# #' inspect.frames(paintedParticles)
# #' }
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# add.contours <- function(raw.frames,
#                           binary.frames=NULL,
#                           trajectories=NULL,
#                           trajIds=NULL,
#                           mode="particles", # could assume also "trajectories" as a value
#                           col=NULL, 
#                           channel=NULL)
# {
#   # store for combining steps
#   input.frames <- raw.frames # and leave untouched
#   # check whether the input has more than one channel in the FrameList
#   if(is(raw.frames,"FrameList"))
#   {
#     nrChannels <- numberOfFrames(raw.frames[[1]]$image)
#     if(nrChannels > 1)
#     {
#       raw.frames <- channels(raw.frames)
#     } #else, it has already just one channel and we can proceed
#   }
#   
#   # if no preprocessed is provided...
#   if(is.null(binary.frames))
#   {
#     # compute them with the defaults
#     if(is(raw.frames,"ChannelsFrameList"))
#     {
#       if(is.null(channel))
#       {
#         stop("You need to provide a channel along with a ChannelsFrameList - or maybe you supplied a FrameList object with more than one channel to the raw.frames parameter?")
#       }
# #     framelistRaw <- raw.frames[[channel]]
#       binary.frames <- preprocess.ChannelsFrameList(raw.frames,channel)
#     } else {
#       binary.frames <- preprocess.FrameList(raw.frames)
#     }
#     # check also whether trajectories are there already, if so throw an error # or a warning?
#     if(!is.null(trajectories))
#     {
#       stop("You are providing a TrajectoryList object, but no binary.frames FrameList!
#            Please check whether you might have it in your workspace")
#     }
#   }
# 
#   # 
#   if(mode=="trajectories") # additional checks on the trajectorylist object provided 
#   {
#     # if no trajectorylist is provided, compute it
#     if(is.null(trajectories))
#     {
#       trajectories <- trajectories(particles(input.frames,binary.frames))
#     }
#     
#     availableIDs <- unlist(lapply(trajectories,function(arg){arg$ID}))
#     # if no single ids are provided, do it for all
#     if(is.null(trajIds))
#     {
#       # will do it on all, so update it to all the available ones
#       trajIds <- availableIDs
#    
#     } else {
#       # will do it by looping on the length of the provided trajectory IDs vector
#       # check that the trajectories are indeed "plottable"!
#       if(!all(trajIds %in% availableIDs))
#       {
#         stop("You are providing IDs of trajectories that are not available. 
#              Please run unlist(lapply(trajectories,function(arg){arg$ID})) on the trajectory object!")
#       }
#     }
#     
#     # actually does it for both cases above
#     colcols <- rep(colorRamps::primary.colors(40,steps=10,FALSE),6)
#     out <- input.frames
#     for(i in trajIds)
#     {
#       currentTraj <- trajectories[[i]]$trajectory
#       counter <- 1
#       for(j in currentTraj$frame)
#       {
#         rawimg <- out[[j]]$image
#         segmimg <- binary.frames[[j]]$image
#         singleObjectSegm <- segmimg
#         singleObjectSegm[segmimg!=currentTraj$frameobjectID[counter]] <- 0
# 
#         rawWithPaintedObj <- paintObjects(singleObjectSegm,rawimg,col=colcols[i])
# 
#         out[[j]]$image <- rawWithPaintedObj 
#         counter <- counter +1
#       }
#     }  
# 
#     
#   } else if(mode=="particles") # there is actually no need for the trajectorylist/trajId, and the col can be just one value ;)
#   {
#     out <- combine.preprocessedFrameList(input.frames,binary.frames,col)
#    
#   } else {
#     stop("The mode parameter must assume one value of the following: 'particles' or 'trajectories'!")
#   }
#     
#   return(out)
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
