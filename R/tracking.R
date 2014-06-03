link.ParticleList <- function(particlelist,
                              L,
                              R=2,
                              epsilon1=0.1,
                              epsilon2=2,
                              lambda1=1,
                              lambda2=1,
                              nframes, # could be used to restrict the range? frameStart to frameEnd?
                              verboseOutput=F
                              )
{
  out <- particlelist
  # preliminary step to set the particles next arrays (or matrix in our case) according to the linkrange
  # done now in case linkrange can get modified during the runs or across different runs
  if(verboseOutput) cat("Starting up...\n\n")
  frames_number <- length(out)
  linkrange <- R
  for (fr in 1:frames_number)
  {
    out[[fr]]$nxt <- matrix(0,nrow=nrow(out[[fr]]$particles),ncol=R)
  }
  
  curr_linkrange <- R # as it can be updated afterwards
  displacement <- L
  # we need at least two frames
  if(frames_number < 2) 
  {
    # error
#     cat("At least",R+1,"frames are needed!!! - INTERRUPTING!!!\n")
    stop("At least",R+1,"frames are needed!!! - INTERRUPTING!!!\n")
  }
  
  # if the linkrange is too big, set it to the right value
  if(frames_number < (curr_linkrange+1))
  {
    curr_linkrange <- frames_number - 1
  }
  
  # max cost = displacement^2
  for(m in 1:(frames_number - curr_linkrange + 1)) # +2?!?!
  {
    cat("---------- Linking frame",m,"of",frames_number,"...........\n")
    nop <- nrow(out[[m]]$particles)
    out[[m]]$special[] <- FALSE
    out[[m]]$nxt[,] <- -1
    
    
    # AND HERE BEGINS THE "TRUE" CYCLE THROUGHOUT NEXT FRAMES
    iterate <- 0
    for(n in 1:curr_linkrange)
    {
      iterate <- 0
      max_cost <- n*displacement*displacement
      
      nop_next <- nrow(out[[m+n]]$particles)
      
      ## cost matrix
      # setup the cost matrix
      C <- n*displacement*displacement*matrix(1,nop+1,nop_next+1)
      ## PLUS VARIATION
      # quadratic distance between p_i and q_j
      xrep <- repmat(t(out[[m+n]]$particles[,1]),nop,1) # function repmat replicates the matlab behaviour
      yrep <- repmat(t(out[[m+n]]$particles[,2]),nop,1)
      xdiff <- xrep - repmat(out[[m]]$particles[,1],1,nop_next)
      ydiff <- yrep - repmat(out[[m]]$particles[,2],1,nop_next)
      deltaSquared <- xdiff^2 + ydiff^2
      alpha <- atan2(ydiff,xdiff)
      
      
      # edit 2014-05-08
      deltaSquared <- (deltaSquared - epsilon2) * ((deltaSquared - epsilon2) > 0)
      # this allows to have a mini jitter on the position of the particle, that can now go slightly backwards and or move orthogonally to the flow
      
      
      
      ## similarly, include the part where the area/intensity is considered
      
      areaVariation <- 0
      # using the ... 6th column, corresponding to the area
      areaDiff <- repmat(t(out[[m+n]]$particles[,"cell.0.s.area"]),nop,1) - repmat(out[[m]]$particles[,"cell.0.s.area"],1,nop_next)
      areaVariation <- areaDiff^2
      
      intensityVariation <- 0
      # using the ... corresponding column, corresponding to the mean intensity
      intDiff <- repmat(t(out[[m+n]]$particles[,"cell.a.b.mean"]),nop,1) - repmat(out[[m]]$particles[,"cell.a.b.mean"],1,nop_next)
      intensityVariation <- intDiff^2
      
      
      # possibly add also a term for the intensity
      
      distFunction <- deltaSquared + areaVariation + intensityVariation
      
      newCost <- lambda1 * (distFunction / (1-lambda2*(alpha/(pi+epsilon1))))
      #       newCost <- deltaSquared
      # cost function for link p_i, q_j
      
      # for everything apart from the dummies, already set to rL^2...
      C[1:nop,1:nop_next] <- newCost # + squared moments and so on
      C[nop+1,nop_next+1] <- 0 # dummy to dummy      
      C[C>max_cost] <- Inf
      
      ## association matrix
      # empty association matrix  
      A <- matrix(0,nrow=nop+1,ncol=nop_next+1)
      # initialize link matrix A --- oldstyle, but working ;) and probably even more optimized than the for for for loops
      for(i in 1:nop)
      {
        # sort costs of real particles
        srtcst <- sort(C[i,])
        srtidx <- order(C[i,])
        # append index of dummy
        iidx <- 1
        dumidx <- which(srtidx==nop_next+1)
        # search for available particle of smallest cost or dummy
        while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
        {
          iidx <- iidx +1      
        }
        A[i,srtidx[iidx]] <- 1
      }
      
      # set dummy particle for columns with no entry
      s <- colSums(A) # or colSums here?!
      A[nop+1,which(s<1)] <- 1
      #   s <- colSums(A)
      #   A[which(s<1),n+1] <- 1
      # dummy corresponds to dummy
      A[nop+1,nop_next+1] <- 1
      # consistency checks
      s1 <- colSums(A[,1:nop_next])
      s2 <- rowSums(A[1:nop,])
      
      # optimize the relation matrix
      finished <- 0
      mincost <- c()
      while(!finished)
      {
        iterate <- iterate + 1
        if(verboseOutput) cat("Frame:",m,"\t---\tLink_range:",n,"\t---\tIteration",iterate,".....")
        # non-set links of finite costs
        todo <- intersect(which(A[1:nop,1:nop_next]==0),which(C[1:nop,1:nop_next]<Inf) )
        
        Icand = ((todo-1) %% nop) + 1
        Jcand = floor((todo-1) / nop) + 1
        
        # determine the reduced cost Cred for each candidate insertion
        # initialize
        Cred <- rep(0,length(todo))
        Xcand <- rep(0,length(todo))
        Ycand <- rep(0,length(todo))
        # compute iteratively
        for(ic in 1:length(Icand))
        {
          Xcand[ic] <- which(A[Icand[ic],]==1)
          Ycand[ic] <- which(A[,Jcand[ic]]==1)
          Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        }
        
        # find minimum cost of corresponding action
        minc <- sort(Cred)[1]
        mini <- order(Cred)[1]
        mincost <- c(mincost,minc)
        
        # if minimum < 0, link addition is favorable
        if(minc < 0)
        {
          if(verboseOutput) cat("--> Performing change!\n")
          # add link and update dependencies to preserve the topology
          A[Icand[mini],Jcand[mini]] <- 1
          A[Ycand[mini],Jcand[mini]] <- 0
          A[Icand[mini],Xcand[mini]] <- 0
          A[Ycand[mini],Xcand[mini]] <- 1
          
        } else {
          # the best is already done and there is no room for improving the cost function
          finished <- TRUE
          if(verboseOutput) cat("--> Found OPTIMAL solution for the current cost function!\n")
        }
        
        # consistency checks--..
        s1 <- colSums(A[,1:nop_next])
        s2 <- rowSums(A[1:nop,])
      }
      
      # convert link matrix to representation in the list format
      links <- which(A[1:nop,]==1,arr.ind=T)
      Ilinks <- links[,1]
      Jlinks <- links[,2]
      # if link is to dummy particle, set index to -1
      Jlinks[which(Jlinks==(nop_next+1))] <- -1
      
      # set links in the list object
      out[[m]]$nxt[Ilinks,n] <- Jlinks
    }
    # shrink curr_linkrange if needed at the end of the frames
    if(m==(frames_number-curr_linkrange-1) && curr_linkrange > 1)
      curr_linkrange <- curr_linkrange - 1
    
  }
  #   cat("I GOT HERE ----")
  # terminate all links at the list objects at the very last frame
  out[[frames_number]]$special[] <- FALSE
  out[[frames_number]]$nxt[,] <- -1  
  
  
  
  class(out) <- c("linkedParticleList",class(out))
  return(out)  
}










############## from here downwards, it's "just" old functions





















runFrames <- function(frameStart,frameEnd,movementDirection="leftToRight")
{
  # initializing the labels for tracking
  iamtracking <- imgObjectsDF
  
  iamtracking[iamtracking$frame==1,]$label <- 1:sum(iamtracking$frame==1)
  iamtracking[iamtracking$frame==1,]$processed <- TRUE
  iamtracking[iamtracking$frame==1,]$startOfTrack <- TRUE
  
  maximumPixelDistance <- 60
  maximumYdisplacement <- 10
  maximumBackwardsDisplacement <- 5
  # as we can exploit the fact that cells are flowing in one direction (in our case, to the left)
  
  
  # for(f in 1:(length(candidate_feats_green)-1)) # to the last of the frames actually (-1)
  # for(f in 1:10) # to the last of the frames actually (-1)
  for(f in frameStart:(frameEnd-1)) # to the last of the frames actually (-1)
  {
    cat("FRAME",f,"under analysis\n----------------------\n")
    
    cat("errorPoint A\n")
    # define the frames to work on
    currentFrame <- iamtracking[iamtracking$frame==f,]
    nextFrame <- iamtracking[iamtracking$frame==f+1,]
    
    # initialize the distance matrices
    eucliDist <- matrix(NA,nrow=nrow(currentFrame),ncol=nrow(nextFrame))
    dists2 <- eucliDist
    ydispl <- eucliDist
    xdispl <- eucliDist
    backdispl <- eucliDist
    
    
    cat("errorPoint A2\n")
    #     browser()
    # calculate distances & co
    for (i in 1:nrow(currentFrame))
    {
      for (j in 1:nrow(nextFrame))
      {
        #         cat(j,"\t")
        frameMeasurements <- calcDist2(currentFrame[i,],nextFrame[j,],movementDirection=movementDirection)
        cat()
        dists2[i,j] <- frameMeasurements[[1]]
        xdispl[i,j] <- frameMeasurements[[2]]
        ydispl[i,j] <- frameMeasurements[[3]]
        backdispl[i,j] <- frameMeasurements[[4]]
      }
    }
    cat("errorPoint B\n")
    
    assignedCounter <- 0
    # scrolling through dists2... i.e. checking row by row from the current frame to the next
    for (i in 1:nrow(currentFrame))
    {
      # the track is the one of the row under analysis
      currentTrack <- currentFrame$label[i]
      # order the distances with the others
      sorter <- order(dists2[i,])
      sortedDistances <- dists2[i,][sorter]
      corresponding_yDisplacement <- ydispl[i,][sorter]
      corresponding_backwardsDisplacement <- backdispl[i,][sorter]
      alreadyUsed <- which(iamtracking[iamtracking$frame==f+1,][sorter,]$processed==TRUE)
      
      # shrinking the possibilities...
      discarded <- unique(c(which(corresponding_yDisplacement > maximumYdisplacement),
                            which(corresponding_backwardsDisplacement > maximumBackwardsDisplacement),
                            alreadyUsed))
      #     cat("bp1",i,"\n")#;browser()
      if(!is.null(discarded))
      {
        candidates <- sortedDistances[-discarded]
      } else
        candidates <- sortedDistances
      #     cat(candidates)
      
      if(length(candidates)>0)
      {
        if (candidates[1] <= maximumPixelDistance)
        {
          #         iamtracking$label[iamtracking$frame==f+1,][which(dists2[i,]==candidates[1])] <- currentTrack
          ## THIS is the assignment
          #         browser()
          iamtracking[iamtracking$frame==f+1,][sorter,][-discarded,]$label[1]  <- currentTrack
          iamtracking[iamtracking$frame==f+1,][sorter,][-discarded,]$processed[1]  <- TRUE
          assignedCounter <- assignedCounter + 1
          # this that follows is just an update
          #         yetToBeAssigned <- yetToBeAssigned[-which(row.names(yetToBeAssigned)==row.names(iamtracking[iamtracking$frame==f+1,][sorter,][-discarded,])),]
          #         labeledToBeMatched <- labeledToBeMatched[-which(labeledToBeMatched$label==currentTrack),]
          #       
        } else # noone is in the nearbies
        {
          #         cat("   ------    noone in the nearbies -> track broken\n")
        }
      } else # no candidates are there - because no object respects the imposed rules
      {
        #       cat("no candidates found!")
        # no candidates are left, the track is broken there
        #       cat("track broken\n")
      }
    }  
    cat("errorPoint C\n")
    
    cat("No more candidates left, moving on to next frame!\n\n")
    # but before this, assign tracks to the nextframe incrementally.
    largestTrackIndex <- max(iamtracking$label,na.rm=TRUE)
    startFrom <- largestTrackIndex + 1
    proceedForNSteps <- nrow(nextFrame)-assignedCounter # for the ones that are there, but no match was found
    wereNAs <- is.na(iamtracking$label[iamtracking$frame==f+1])
    if(proceedForNSteps > 0)
    {
      iamtracking$label[iamtracking$frame==f+1][wereNAs] <- seq(from=startFrom,length.out=proceedForNSteps)
      iamtracking$processed[iamtracking$frame==f+1][wereNAs] <- TRUE
      iamtracking$startOfTrack[iamtracking$frame==f+1][wereNAs] <- TRUE
    }
  }
  return(iamtracking)
}
# iamtracking <- runFrames(frameStart=1,frameEnd=100)
# library("rgl")
# plot3d(iamtracking$cell.0.m.cx, iamtracking$cell.0.m.cy, iamtracking$frame, col=iamtracking$label)




linkParticles <- function(imgObjectList,L,R=1)
{
  nframes <- length(candidate_feats_red)
  
  
  
  for(iframe in 2:nframes)
  {
    imgObjectsList[[iframe-1]]$link <- -1
    cat("\n----------------------------\nLinking paths in frame",iframe,"of",nframes,"\n")
    
    # number of particles detected in current and next frame
    m <- nrow(imgObjectsList[[iframe-1]])
    n <- nrow(imgObjectsList[[iframe]])
    
    # empty association matrix
    A <- matrix(0,nrow=m+1,ncol=n+1)
    
    # quadratic distance between p_i and q_j
    xrep <- repmat(t(imgObjectsList[[iframe]][,1]),m,1) # function repmat replicates the matlab behaviour
    yrep <- repmat(t(imgObjectsList[[iframe]][,2]),m,1)
    xdiff <- xrep - repmat(imgObjectsList[[iframe-1]][,1],1,n)
    ydiff <- yrep - repmat(imgObjectsList[[iframe-1]][,2],1,n)
    delta <- xdiff^2 + ydiff^2
    
    # cost function for link p_i, q_j
    
    #     L <- 100 #highest distance traveled across one frame
    C <- R*L*L*matrix(1,m+1,n+1)
    C[m+1,n+1] <-0 # dummy to dummy
    C[1:m,1:n] <- delta # + squared moments and so on
    
    # set cost of matchings that will never occur to Inf
    #   C1 <- C[1:m,1:n] - repmat(t(C[m+1,1:n]),m,1)
    #   C2 <- C[1:m,1:n] - repmat(C[1:m,n+1],1,n)
    C[C>R*L*L] <- Inf
    
    # initialize link matrix A
    for(i in 1:m)
    {
      # sort costs of real particles
      srtcst <- sort(C[i,])
      srtidx <- order(C[i,])
      # append index of dummy
      iidx <- 1
      dumidx <- which(srtidx==n+1)
      # search for available particle of smallest cost or dummy
      while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
      {
        iidx <- iidx +1      
      }
      A[i,srtidx[iidx]] <- 1
      
    }
    
    # set dummy particle for columns with no entry
    s <- colSums(A) # or colSums here?!
    A[m+1,which(s<1)] <- 1
    #   s <- colSums(A)
    #   A[which(s<1),n+1] <- 1
    # dummy corresponds to dummy
    A[m+1,n+1] <- 1
    # consistency checks
    s1 <- colSums(A[,1:n])
    s2 <- rowSums(A[1:m,])
    
    A
    #     A[1:15,1:15] <- diag(15) # try to perturb :)
    # iteration loop for the logistic transportation algorithm
    finished <- FALSE
    iterate <- 0
    mincost <- c()
    
    while(!finished)
    {
      iterate <- iterate + 1
      cat("Iteration",iterate,".....")
      # non-set links of finite costs
      todo <- intersect(which(A[1:m,1:n]==0),which(C[1:m,1:n]<Inf) )
      # "coordinates" of the ones still to do
      #       Icand <- rep(0,length(todo))
      #       Jcand <- rep(0,length(todo))
      #       for (itodo in 1:length(todo))
      #       {
      #         Atemp <- matrix(0,m,n)
      #         Atemp[todo[itodo]] <- 1
      #         Icand[itodo] <- which(rowSums(Atemp) != 0)
      #         Jcand[itodo] <- which(colSums(Atemp) != 0)
      #       }
      #       
      Icand = ((todo-1) %% m) + 1
      Jcand = floor((todo-1) / m) + 1
      
      
      # determine the reduced cost Cred for each candidate insertion
      # initialize
      Cred <- rep(0,length(todo))
      Xcand <- rep(0,length(todo))
      Ycand <- rep(0,length(todo))
      # compute iteratively
      for(ic in 1:length(Icand))
      {
        Xcand[ic] <- which(A[Icand[ic],]==1)
        Ycand[ic] <- which(A[,Jcand[ic]]==1)
        Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        #         Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Xcand[ic],Ycand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
      }
      
      # find minimum cost of corresponding action
      minc <- sort(Cred)[1]
      mini <- order(Cred)[1]
      mincost <- c(mincost,minc)
      
      # if minimum < 0, link addition is favorable
      if(minc < 0)
      {
        cat("Performing change!")
        # add link and update dependencies to preserve the topology
        A[Icand[mini],Jcand[mini]] <- 1
        A[Ycand[mini],Jcand[mini]] <- 0
        A[Icand[mini],Xcand[mini]] <- 0
        A[Ycand[mini],Xcand[mini]] <- 1
        
      } else {
        # the best is already done and there is no room for improving the cost function
        finished <- TRUE
      }
      
      # consistency checks--..
      s1 <- colSums(A[,1:n])
      s2 <- rowSums(A[1:m,])
    }
    
    # convert link matrix to representation in the list format
    links <- which(A[1:m,]==1,arr.ind=T)
    Ilinks <- links[,1]
    Jlinks <- links[,2]
    # if link is to dummy particle, set index to -1
    Jlinks[which(Jlinks==(n+1))] <- -1
    
    # set links in the list object
    imgObjectsList[[iframe-1]]$link[Ilinks] <- Jlinks
  }
  
  # terminate all links at the list objects at the very last frame
  imgObjectsList[[nframes]]$link <- -1
  return(imgObjectsList)
}





##### with modified cost function
linkParticles_v2 <- function(imgObjectList,L,R=1,epsilon=0.1,lambda1=1,lambda2=1)
{
  nframes <- length(candidate_feats_red)
  
  for(iframe in 2:nframes)
  {
    imgObjectsList[[iframe-1]]$link <- -1
    cat("\n----------------------------\nLinking paths in frame",iframe,"of",nframes,"\n")
    
    # number of particles detected in current and next frame
    m <- nrow(imgObjectsList[[iframe-1]])
    n <- nrow(imgObjectsList[[iframe]])
    
    # empty association matrix
    A <- matrix(0,nrow=m+1,ncol=n+1)
    
    ## PLUS VARIATION
    # quadratic distance between p_i and q_j
    xrep <- repmat(t(imgObjectsList[[iframe]][,1]),m,1) # function repmat replicates the matlab behaviour
    yrep <- repmat(t(imgObjectsList[[iframe]][,2]),m,1)
    xdiff <- xrep - repmat(imgObjectsList[[iframe-1]][,1],1,n)
    ydiff <- yrep - repmat(imgObjectsList[[iframe-1]][,2],1,n)
    deltaSquared <- xdiff^2 + ydiff^2
    
    alpha <- atan2(ydiff,xdiff)
    #     newCost <- lambda1 * (deltaSquared / (1-lambda2*(alpha/(pi+epsilon))))
    newCost <- deltaSquared 
    # cost function for link p_i, q_j
    
    #     L <- 100 #highest distance traveled across one frame
    C <- R*L*L*matrix(1,m+1,n+1)
    
    C[1:m,1:n] <- newCost # + squared moments and so on
    
    # set cost of matchings that will never occur to Inf
    #   C1 <- C[1:m,1:n] - repmat(t(C[m+1,1:n]),m,1)
    #   C2 <- C[1:m,1:n] - repmat(C[1:m,n+1],1,n)
    C[C>R*L*L] <- Inf
    
    # initialize link matrix A
    for(i in 1:m)
    {
      # sort costs of real particles
      srtcst <- sort(C[i,])
      srtidx <- order(C[i,])
      # append index of dummy
      iidx <- 1
      dumidx <- which(srtidx==n+1)
      # search for available particle of smallest cost or dummy
      while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
      {
        iidx <- iidx +1      
      }
      A[i,srtidx[iidx]] <- 1
      
    }
    
    # set dummy particle for columns with no entry
    s <- colSums(A) # or colSums here?!
    A[m+1,which(s<1)] <- 1
    #   s <- colSums(A)
    #   A[which(s<1),n+1] <- 1
    # dummy corresponds to dummy
    A[m+1,n+1] <- 1
    # consistency checks
    s1 <- colSums(A[,1:n])
    s2 <- rowSums(A[1:m,])
    
    A
    #     A[1:15,1:15] <- diag(15) # try to perturb :)
    # iteration loop for the logistic transportation algorithm
    finished <- FALSE
    iterate <- 0
    mincost <- c()
    
    while(!finished)
    {
      iterate <- iterate + 1
      cat("Iteration",iterate,".....")
      # non-set links of finite costs
      todo <- intersect(which(A[1:m,1:n]==0),which(C[1:m,1:n]<Inf) )
      # "coordinates" of the ones still to do
      #       Icand <- rep(0,length(todo))
      #       Jcand <- rep(0,length(todo))
      #       for (itodo in 1:length(todo))
      #       {
      #         Atemp <- matrix(0,m,n)
      #         Atemp[todo[itodo]] <- 1
      #         Icand[itodo] <- which(rowSums(Atemp) != 0)
      #         Jcand[itodo] <- which(colSums(Atemp) != 0)
      #       }
      #       
      Icand = ((todo-1) %% m) + 1
      Jcand = floor((todo-1) / m) + 1
      
      
      # determine the reduced cost Cred for each candidate insertion
      # initialize
      Cred <- rep(0,length(todo))
      Xcand <- rep(0,length(todo))
      Ycand <- rep(0,length(todo))
      # compute iteratively
      for(ic in 1:length(Icand))
      {
        Xcand[ic] <- which(A[Icand[ic],]==1)
        Ycand[ic] <- which(A[,Jcand[ic]]==1)
        Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        #         Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Xcand[ic],Ycand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
      }
      
      # find minimum cost of corresponding action
      minc <- sort(Cred)[1]
      mini <- order(Cred)[1]
      mincost <- c(mincost,minc)
      
      # if minimum < 0, link addition is favorable
      if(minc < 0)
      {
        cat("Performing change!")
        # add link and update dependencies to preserve the topology
        A[Icand[mini],Jcand[mini]] <- 1
        A[Ycand[mini],Jcand[mini]] <- 0
        A[Icand[mini],Xcand[mini]] <- 0
        A[Ycand[mini],Xcand[mini]] <- 1
        
      } else {
        # the best is already done and there is no room for improving the cost function
        finished <- TRUE
      }
      
      # consistency checks--..
      s1 <- colSums(A[,1:n])
      s2 <- rowSums(A[1:m,])
    }
    
    # convert link matrix to representation in the list format
    links <- which(A[1:m,]==1,arr.ind=T)
    Ilinks <- links[,1]
    Jlinks <- links[,2]
    # if link is to dummy particle, set index to -1
    Jlinks[which(Jlinks==(n+1))] <- -1
    
    # set links in the list object
    imgObjectsList[[iframe-1]]$link[Ilinks] <- Jlinks
  }
  
  # terminate all links at the list objects at the very last frame
  imgObjectsList[[nframes]]$link <- -1
  return(imgObjectsList)
}

##### with modified cost function
linkParticles_v2mod <- function(imgObjectList,L,R=1,epsilon=0.1,lambda1=1,lambda2=1)
{
  nframes <- length(candidate_feats_red)
  
  for(iframe in 2:nframes)
  {
    imgObjectsList[[iframe-1]]$link <- -1
    cat("\n----------------------------\nLinking paths in frame",iframe,"of",nframes,"\n")
    
    # number of particles detected in current and next frame
    m <- nrow(imgObjectsList[[iframe-1]])
    n <- nrow(imgObjectsList[[iframe]])
    
    # empty association matrix
    A <- matrix(0,nrow=m+1,ncol=n+1)
    
    ## PLUS VARIATION
    # quadratic distance between p_i and q_j
    xrep <- repmat(t(imgObjectsList[[iframe]][,1]),m,1) # function repmat replicates the matlab behaviour
    yrep <- repmat(t(imgObjectsList[[iframe]][,2]),m,1)
    xdiff <- xrep - repmat(imgObjectsList[[iframe-1]][,1],1,n)
    ydiff <- yrep - repmat(imgObjectsList[[iframe-1]][,2],1,n)
    deltaSquared <- xdiff^2 + ydiff^2
    
    alpha <- atan2(ydiff,xdiff)
    newCost <- lambda1 * (deltaSquared / (1-lambda2*(alpha/(pi+epsilon))))
    #     newCost <- deltaSquared 
    # cost function for link p_i, q_j
    
    #     L <- 100 #highest distance traveled across one frame
    C <- R*L*L*matrix(1,m+1,n+1)
    
    C[1:m,1:n] <- newCost # + squared moments and so on
    
    # set cost of matchings that will never occur to Inf
    #   C1 <- C[1:m,1:n] - repmat(t(C[m+1,1:n]),m,1)
    #   C2 <- C[1:m,1:n] - repmat(C[1:m,n+1],1,n)
    C[C>R*L*L] <- Inf
    
    # initialize link matrix A
    for(i in 1:m)
    {
      # sort costs of real particles
      srtcst <- sort(C[i,])
      srtidx <- order(C[i,])
      # append index of dummy
      iidx <- 1
      dumidx <- which(srtidx==n+1)
      # search for available particle of smallest cost or dummy
      while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
      {
        iidx <- iidx +1      
      }
      A[i,srtidx[iidx]] <- 1
      
    }
    
    # set dummy particle for columns with no entry
    s <- colSums(A) # or colSums here?!
    A[m+1,which(s<1)] <- 1
    #   s <- colSums(A)
    #   A[which(s<1),n+1] <- 1
    # dummy corresponds to dummy
    A[m+1,n+1] <- 1
    # consistency checks
    s1 <- colSums(A[,1:n])
    s2 <- rowSums(A[1:m,])
    
    A
    #     A[1:15,1:15] <- diag(15) # try to perturb :)
    # iteration loop for the logistic transportation algorithm
    finished <- FALSE
    iterate <- 0
    mincost <- c()
    
    while(!finished)
    {
      iterate <- iterate + 1
      cat("Iteration",iterate,".....")
      # non-set links of finite costs
      todo <- intersect(which(A[1:m,1:n]==0),which(C[1:m,1:n]<Inf) )
      # "coordinates" of the ones still to do
      #       Icand <- rep(0,length(todo))
      #       Jcand <- rep(0,length(todo))
      #       for (itodo in 1:length(todo))
      #       {
      #         Atemp <- matrix(0,m,n)
      #         Atemp[todo[itodo]] <- 1
      #         Icand[itodo] <- which(rowSums(Atemp) != 0)
      #         Jcand[itodo] <- which(colSums(Atemp) != 0)
      #       }
      #       
      Icand = ((todo-1) %% m) + 1
      Jcand = floor((todo-1) / m) + 1
      
      
      # determine the reduced cost Cred for each candidate insertion
      # initialize
      Cred <- rep(0,length(todo))
      Xcand <- rep(0,length(todo))
      Ycand <- rep(0,length(todo))
      # compute iteratively
      for(ic in 1:length(Icand))
      {
        Xcand[ic] <- which(A[Icand[ic],]==1)
        Ycand[ic] <- which(A[,Jcand[ic]]==1)
        Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        #         Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Xcand[ic],Ycand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
      }
      
      # find minimum cost of corresponding action
      minc <- sort(Cred)[1]
      mini <- order(Cred)[1]
      mincost <- c(mincost,minc)
      
      # if minimum < 0, link addition is favorable
      if(minc < 0)
      {
        cat("Performing change!")
        # add link and update dependencies to preserve the topology
        A[Icand[mini],Jcand[mini]] <- 1
        A[Ycand[mini],Jcand[mini]] <- 0
        A[Icand[mini],Xcand[mini]] <- 0
        A[Ycand[mini],Xcand[mini]] <- 1
        
      } else {
        # the best is already done and there is no room for improving the cost function
        finished <- TRUE
      }
      
      # consistency checks--..
      s1 <- colSums(A[,1:n])
      s2 <- rowSums(A[1:m,])
    }
    
    # convert link matrix to representation in the list format
    links <- which(A[1:m,]==1,arr.ind=T)
    Ilinks <- links[,1]
    Jlinks <- links[,2]
    # if link is to dummy particle, set index to -1
    Jlinks[which(Jlinks==(n+1))] <- -1
    
    # set links in the list object
    imgObjectsList[[iframe-1]]$link[Ilinks] <- Jlinks
  }
  
  # terminate all links at the list objects at the very last frame
  imgObjectsList[[nframes]]$link <- -1
  return(imgObjectsList)
}





##### with scanning through all other allowed range of next frames
linkParticles_v3 <- function(particleList=imgObjectsList_v3,
                             L,
                             R=2,
                             epsilon=0.1,
                             lambda1=1,
                             lambda2=1,
                             nframes=length(candidate_feats_red),
                             verboseOutput=F
)
{
  # preliminary step to set the particles next arrays (or matrix in our case) according to the linkrange
  # done now in case linkrange can get modified during the runs or across different runs
  if(verboseOutput) cat("Starting up...\n\n")
  frames_number <- nframes
  linkrange <- R
  for (fr in 1:frames_number)
  {
    particleList[[fr]]$nxt <- matrix(0,nrow=nrow(particleList[[fr]]$particles),ncol=R)
  }
  
  curr_linkrange <- R
  displacement <- L
  # we need at least two frames
  if(frames_number < 2) 
  {
    # error
    cat("At least",R+1,"frames are needed!!! - INTERRUPTING!!!\n")
  }
  
  # if the linkrange is too big, set it to the right value
  if(frames_number < (curr_linkrange+1))
  {
    curr_linkrange <- frames_number - 1
  }
  
  # max cost = displacement^2
  for(m in 1:(frames_number - curr_linkrange + 1)) # +2?!?!
  {
    cat("---------- Linking frame",m,"of",frames_number,"...........\n")
    nop <- nrow(particleList[[m]]$particles)
    particleList[[m]]$special[] <- FALSE
    particleList[[m]]$nxt[,] <- -1
    
    
    # AND HERE BEGINS THE "TRUE" CYCLE THROUGHOUT NEXT FRAMES
    iterate <- 0
    for(n in 1:curr_linkrange)
    {
      iterate <- 0
      max_cost <- n*displacement*displacement
      
      nop_next <- nrow(particleList[[m+n]]$particles)
      
      ## cost matrix
      # setup the cost matrix
      C <- n*displacement*displacement*matrix(1,nop+1,nop_next+1)
      ## PLUS VARIATION
      # quadratic distance between p_i and q_j
      xrep <- repmat(t(particleList[[m+n]]$particles[,1]),nop,1) # function repmat replicates the matlab behaviour
      yrep <- repmat(t(particleList[[m+n]]$particles[,2]),nop,1)
      xdiff <- xrep - repmat(particleList[[m]]$particles[,1],1,nop_next)
      ydiff <- yrep - repmat(particleList[[m]]$particles[,2],1,nop_next)
      deltaSquared <- xdiff^2 + ydiff^2
      alpha <- atan2(ydiff,xdiff)
      newCost <- lambda1 * (deltaSquared / (1-lambda2*(alpha/(pi+epsilon))))
      #       newCost <- deltaSquared
      # cost function for link p_i, q_j
      
      # for everything apart from the dummies, already set to rL^2...
      C[1:nop,1:nop_next] <- newCost # + squared moments and so on
      C[nop+1,nop_next+1] <- 0 # dummy to dummy      
      C[C>max_cost] <- Inf
      
      ## association matrix
      # empty association matrix  
      A <- matrix(0,nrow=nop+1,ncol=nop_next+1)
      # initialize link matrix A --- oldstyle, but working ;) and probably even more optimized than the for for for loops
      for(i in 1:nop)
      {
        # sort costs of real particles
        srtcst <- sort(C[i,])
        srtidx <- order(C[i,])
        # append index of dummy
        iidx <- 1
        dumidx <- which(srtidx==nop_next+1)
        # search for available particle of smallest cost or dummy
        while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
        {
          iidx <- iidx +1      
        }
        A[i,srtidx[iidx]] <- 1
      }
      
      # set dummy particle for columns with no entry
      s <- colSums(A) # or colSums here?!
      A[nop+1,which(s<1)] <- 1
      #   s <- colSums(A)
      #   A[which(s<1),n+1] <- 1
      # dummy corresponds to dummy
      A[nop+1,nop_next+1] <- 1
      # consistency checks
      s1 <- colSums(A[,1:nop_next])
      s2 <- rowSums(A[1:nop,])
      
      # optimize the relation matrix
      finished <- 0
      mincost <- c()
      while(!finished)
      {
        iterate <- iterate + 1
        if(verboseOutput) cat("Frame:",m,"\t---\tLink_range:",n,"\t---\tIteration",iterate,".....")
        # non-set links of finite costs
        todo <- intersect(which(A[1:nop,1:nop_next]==0),which(C[1:nop,1:nop_next]<Inf) )
        
        Icand = ((todo-1) %% nop) + 1
        Jcand = floor((todo-1) / nop) + 1
        
        # determine the reduced cost Cred for each candidate insertion
        # initialize
        Cred <- rep(0,length(todo))
        Xcand <- rep(0,length(todo))
        Ycand <- rep(0,length(todo))
        # compute iteratively
        for(ic in 1:length(Icand))
        {
          Xcand[ic] <- which(A[Icand[ic],]==1)
          Ycand[ic] <- which(A[,Jcand[ic]]==1)
          Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        }
        
        # find minimum cost of corresponding action
        minc <- sort(Cred)[1]
        mini <- order(Cred)[1]
        mincost <- c(mincost,minc)
        
        # if minimum < 0, link addition is favorable
        if(minc < 0)
        {
          if(verboseOutput) cat("--> Performing change!\n")
          # add link and update dependencies to preserve the topology
          A[Icand[mini],Jcand[mini]] <- 1
          A[Ycand[mini],Jcand[mini]] <- 0
          A[Icand[mini],Xcand[mini]] <- 0
          A[Ycand[mini],Xcand[mini]] <- 1
          
        } else {
          # the best is already done and there is no room for improving the cost function
          finished <- TRUE
          if(verboseOutput) cat("--> Found OPTIMAL solution for the current cost function!\n")
        }
        
        # consistency checks--..
        s1 <- colSums(A[,1:nop_next])
        s2 <- rowSums(A[1:nop,])
      }
      
      # convert link matrix to representation in the list format
      links <- which(A[1:nop,]==1,arr.ind=T)
      Ilinks <- links[,1]
      Jlinks <- links[,2]
      # if link is to dummy particle, set index to -1
      Jlinks[which(Jlinks==(nop_next+1))] <- -1
      
      # set links in the list object
      particleList[[m]]$nxt[Ilinks,n] <- Jlinks
    }
    # shrink curr_linkrange if needed at the end of the frames
    if(m==(frames_number-curr_linkrange-1) && curr_linkrange > 1)
      curr_linkrange <- curr_linkrange - 1
    
  }
  #   cat("I GOT HERE ----")
  # terminate all links at the list objects at the very last frame
  particleList[[frames_number]]$special[] <- FALSE
  particleList[[frames_number]]$nxt[,] <- -1  
  
  
  
  
  return(particleList)  
}


##### with scanning through all other allowed range of next frames
# with intensity included in the distance calculation - TRIAL 1
linkParticles_v4_withIntensity <- function(particleList=imgObjectsList_v3,
                                           L,
                                           R=2,
                                           epsilon=0.1,
                                           lambda1=1,
                                           lambda2=1,
                                           nframes=length(candidate_feats_red),
                                           verboseOutput=F
)
{
  # preliminary step to set the particles next arrays (or matrix in our case) according to the linkrange
  # done now in case linkrange can get modified during the runs or across different runs
  if(verboseOutput) cat("Starting up...\n\n")
  frames_number <- nframes
  linkrange <- R
  for (fr in 1:frames_number)
  {
    particleList[[fr]]$nxt <- matrix(0,nrow=nrow(particleList[[fr]]$particles),ncol=R)
  }
  
  curr_linkrange <- R
  displacement <- L
  # we need at least two frames
  if(frames_number < 2) 
  {
    # error
    cat("At least",R+1,"frames are needed!!! - INTERRUPTING!!!\n")
  }
  
  # if the linkrange is too big, set it to the right value
  if(frames_number < (curr_linkrange+1))
  {
    curr_linkrange <- frames_number - 1
  }
  
  # max cost = displacement^2
  for(m in 1:(frames_number - curr_linkrange + 1)) # +2?!?!
  {
    cat("---------- Linking frame",m,"of",frames_number,"...........\n")
    nop <- nrow(particleList[[m]]$particles)
    particleList[[m]]$special[] <- FALSE
    particleList[[m]]$nxt[,] <- -1
    
    
    # AND HERE BEGINS THE "TRUE" CYCLE THROUGHOUT NEXT FRAMES
    iterate <- 0
    for(n in 1:curr_linkrange)
    {
      iterate <- 0
      max_cost <- n*displacement*displacement
      
      nop_next <- nrow(particleList[[m+n]]$particles)
      
      ## cost matrix
      # setup the cost matrix
      C <- n*displacement*displacement*matrix(1,nop+1,nop_next+1)
      ## PLUS VARIATION
      # quadratic distance between p_i and q_j
      xrep <- repmat(t(particleList[[m+n]]$particles[,1]),nop,1) # function repmat replicates the matlab behaviour
      yrep <- repmat(t(particleList[[m+n]]$particles[,2]),nop,1)
      xdiff <- xrep - repmat(particleList[[m]]$particles[,1],1,nop_next)
      ydiff <- yrep - repmat(particleList[[m]]$particles[,2],1,nop_next)
      deltaSquared <- xdiff^2 + ydiff^2
      alpha <- atan2(ydiff,xdiff)
      
      ## similarly, include the part where the area/intensity is considered
      
      areaVariation <- c()
      
      
      distFunction <- deltaSquared + areaVariation
      
      newCost <- lambda1 * (distFunction / (1-lambda2*(alpha/(pi+epsilon))))
      #       newCost <- deltaSquared
      # cost function for link p_i, q_j
      
      # for everything apart from the dummies, already set to rL^2...
      C[1:nop,1:nop_next] <- newCost # + squared moments and so on
      C[nop+1,nop_next+1] <- 0 # dummy to dummy      
      C[C>max_cost] <- Inf
      
      ## association matrix
      # empty association matrix  
      A <- matrix(0,nrow=nop+1,ncol=nop_next+1)
      # initialize link matrix A --- oldstyle, but working ;) and probably even more optimized than the for for for loops
      for(i in 1:nop)
      {
        # sort costs of real particles
        srtcst <- sort(C[i,])
        srtidx <- order(C[i,])
        # append index of dummy
        iidx <- 1
        dumidx <- which(srtidx==nop_next+1)
        # search for available particle of smallest cost or dummy
        while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
        {
          iidx <- iidx +1      
        }
        A[i,srtidx[iidx]] <- 1
      }
      
      # set dummy particle for columns with no entry
      s <- colSums(A) # or colSums here?!
      A[nop+1,which(s<1)] <- 1
      #   s <- colSums(A)
      #   A[which(s<1),n+1] <- 1
      # dummy corresponds to dummy
      A[nop+1,nop_next+1] <- 1
      # consistency checks
      s1 <- colSums(A[,1:nop_next])
      s2 <- rowSums(A[1:nop,])
      
      # optimize the relation matrix
      finished <- 0
      mincost <- c()
      while(!finished)
      {
        iterate <- iterate + 1
        if(verboseOutput) cat("Frame:",m,"\t---\tLink_range:",n,"\t---\tIteration",iterate,".....")
        # non-set links of finite costs
        todo <- intersect(which(A[1:nop,1:nop_next]==0),which(C[1:nop,1:nop_next]<Inf) )
        
        Icand = ((todo-1) %% nop) + 1
        Jcand = floor((todo-1) / nop) + 1
        
        # determine the reduced cost Cred for each candidate insertion
        # initialize
        Cred <- rep(0,length(todo))
        Xcand <- rep(0,length(todo))
        Ycand <- rep(0,length(todo))
        # compute iteratively
        for(ic in 1:length(Icand))
        {
          Xcand[ic] <- which(A[Icand[ic],]==1)
          Ycand[ic] <- which(A[,Jcand[ic]]==1)
          Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        }
        
        # find minimum cost of corresponding action
        minc <- sort(Cred)[1]
        mini <- order(Cred)[1]
        mincost <- c(mincost,minc)
        
        # if minimum < 0, link addition is favorable
        if(minc < 0)
        {
          if(verboseOutput) cat("--> Performing change!\n")
          # add link and update dependencies to preserve the topology
          A[Icand[mini],Jcand[mini]] <- 1
          A[Ycand[mini],Jcand[mini]] <- 0
          A[Icand[mini],Xcand[mini]] <- 0
          A[Ycand[mini],Xcand[mini]] <- 1
          
        } else {
          # the best is already done and there is no room for improving the cost function
          finished <- TRUE
          if(verboseOutput) cat("--> Found OPTIMAL solution for the current cost function!\n")
        }
        
        # consistency checks--..
        s1 <- colSums(A[,1:nop_next])
        s2 <- rowSums(A[1:nop,])
      }
      
      # convert link matrix to representation in the list format
      links <- which(A[1:nop,]==1,arr.ind=T)
      Ilinks <- links[,1]
      Jlinks <- links[,2]
      # if link is to dummy particle, set index to -1
      Jlinks[which(Jlinks==(nop_next+1))] <- -1
      
      # set links in the list object
      particleList[[m]]$nxt[Ilinks,n] <- Jlinks
    }
    # shrink curr_linkrange if needed at the end of the frames
    if(m==(frames_number-curr_linkrange-1) && curr_linkrange > 1)
      curr_linkrange <- curr_linkrange - 1
    
  }
  #   cat("I GOT HERE ----")
  # terminate all links at the list objects at the very last frame
  particleList[[frames_number]]$special[] <- FALSE
  particleList[[frames_number]]$nxt[,] <- -1  
  
  
  
  
  return(particleList)  
}



## will do some cleanup in this and other files with multiple versions inside - before starting using version control!
linkParticles_v5_withIntensity <- function(particleList=imgObjectsList_v3,
                                           L,
                                           R=2,
                                           epsilon1=0.1,
                                           epsilon2=2,
                                           lambda1=1,
                                           lambda2=1,
                                           nframes=length(candidate_feats_red),
                                           verboseOutput=F
)
{
  # preliminary step to set the particles next arrays (or matrix in our case) according to the linkrange
  # done now in case linkrange can get modified during the runs or across different runs
  if(verboseOutput) cat("Starting up...\n\n")
  frames_number <- nframes
  linkrange <- R
  for (fr in 1:frames_number)
  {
    particleList[[fr]]$nxt <- matrix(0,nrow=nrow(particleList[[fr]]$particles),ncol=R)
  }
  
  curr_linkrange <- R
  displacement <- L
  # we need at least two frames
  if(frames_number < 2) 
  {
    # error
    cat("At least",R+1,"frames are needed!!! - INTERRUPTING!!!\n")
  }
  
  # if the linkrange is too big, set it to the right value
  if(frames_number < (curr_linkrange+1))
  {
    curr_linkrange <- frames_number - 1
  }
  
  # max cost = displacement^2
  for(m in 1:(frames_number - curr_linkrange + 1)) # +2?!?!
  {
    cat("---------- Linking frame",m,"of",frames_number,"...........\n")
    nop <- nrow(particleList[[m]]$particles)
    particleList[[m]]$special[] <- FALSE
    particleList[[m]]$nxt[,] <- -1
    
    
    # AND HERE BEGINS THE "TRUE" CYCLE THROUGHOUT NEXT FRAMES
    iterate <- 0
    for(n in 1:curr_linkrange)
    {
      iterate <- 0
      max_cost <- n*displacement*displacement
      
      nop_next <- nrow(particleList[[m+n]]$particles)
      
      ## cost matrix
      # setup the cost matrix
      C <- n*displacement*displacement*matrix(1,nop+1,nop_next+1)
      ## PLUS VARIATION
      # quadratic distance between p_i and q_j
      xrep <- repmat(t(particleList[[m+n]]$particles[,1]),nop,1) # function repmat replicates the matlab behaviour
      yrep <- repmat(t(particleList[[m+n]]$particles[,2]),nop,1)
      xdiff <- xrep - repmat(particleList[[m]]$particles[,1],1,nop_next)
      ydiff <- yrep - repmat(particleList[[m]]$particles[,2],1,nop_next)
      deltaSquared <- xdiff^2 + ydiff^2
      alpha <- atan2(ydiff,xdiff)
      
      
      # edit 2014-05-08
      deltaSquared <- (deltaSquared - epsilon2) * ((deltaSquared - epsilon2) > 0)
      # this allows to have a mini jitter on the position of the particle, that can now go slightly backwards and or move orthogonally to the flow
      
      
      
      ## similarly, include the part where the area/intensity is considered
      
      areaVariation <- 0
      # using the ... 6th column, corresponding to the area
      areaDiff <- repmat(t(particleList[[m+n]]$particles[,6]),nop,1) - repmat(particleList[[m]]$particles[,6],1,nop_next)
      areaVariation <- areaDiff^2
      
      intensityVariation <- 0
      # using the ... corresponding column, corresponding to the mean intensity
      intDiff <- repmat(t(particleList[[m+n]]$particles[,"cell.a.b.mean"]),nop,1) - repmat(particleList[[m]]$particles[,"cell.a.b.mean"],1,nop_next)
      intensityVariation <- intDiff^2
      
      
      # possibly add also a term for the intensity
      
      distFunction <- deltaSquared + areaVariation + intensityVariation
      
      newCost <- lambda1 * (distFunction / (1-lambda2*(alpha/(pi+epsilon1))))
      #       newCost <- deltaSquared
      # cost function for link p_i, q_j
      
      # for everything apart from the dummies, already set to rL^2...
      C[1:nop,1:nop_next] <- newCost # + squared moments and so on
      C[nop+1,nop_next+1] <- 0 # dummy to dummy      
      C[C>max_cost] <- Inf
      
      ## association matrix
      # empty association matrix  
      A <- matrix(0,nrow=nop+1,ncol=nop_next+1)
      # initialize link matrix A --- oldstyle, but working ;) and probably even more optimized than the for for for loops
      for(i in 1:nop)
      {
        # sort costs of real particles
        srtcst <- sort(C[i,])
        srtidx <- order(C[i,])
        # append index of dummy
        iidx <- 1
        dumidx <- which(srtidx==nop_next+1)
        # search for available particle of smallest cost or dummy
        while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
        {
          iidx <- iidx +1      
        }
        A[i,srtidx[iidx]] <- 1
      }
      
      # set dummy particle for columns with no entry
      s <- colSums(A) # or colSums here?!
      A[nop+1,which(s<1)] <- 1
      #   s <- colSums(A)
      #   A[which(s<1),n+1] <- 1
      # dummy corresponds to dummy
      A[nop+1,nop_next+1] <- 1
      # consistency checks
      s1 <- colSums(A[,1:nop_next])
      s2 <- rowSums(A[1:nop,])
      
      # optimize the relation matrix
      finished <- 0
      mincost <- c()
      while(!finished)
      {
        iterate <- iterate + 1
        if(verboseOutput) cat("Frame:",m,"\t---\tLink_range:",n,"\t---\tIteration",iterate,".....")
        # non-set links of finite costs
        todo <- intersect(which(A[1:nop,1:nop_next]==0),which(C[1:nop,1:nop_next]<Inf) )
        
        Icand = ((todo-1) %% nop) + 1
        Jcand = floor((todo-1) / nop) + 1
        
        # determine the reduced cost Cred for each candidate insertion
        # initialize
        Cred <- rep(0,length(todo))
        Xcand <- rep(0,length(todo))
        Ycand <- rep(0,length(todo))
        # compute iteratively
        for(ic in 1:length(Icand))
        {
          Xcand[ic] <- which(A[Icand[ic],]==1)
          Ycand[ic] <- which(A[,Jcand[ic]]==1)
          Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        }
        
        # find minimum cost of corresponding action
        minc <- sort(Cred)[1]
        mini <- order(Cred)[1]
        mincost <- c(mincost,minc)
        
        # if minimum < 0, link addition is favorable
        if(minc < 0)
        {
          if(verboseOutput) cat("--> Performing change!\n")
          # add link and update dependencies to preserve the topology
          A[Icand[mini],Jcand[mini]] <- 1
          A[Ycand[mini],Jcand[mini]] <- 0
          A[Icand[mini],Xcand[mini]] <- 0
          A[Ycand[mini],Xcand[mini]] <- 1
          
        } else {
          # the best is already done and there is no room for improving the cost function
          finished <- TRUE
          if(verboseOutput) cat("--> Found OPTIMAL solution for the current cost function!\n")
        }
        
        # consistency checks--..
        s1 <- colSums(A[,1:nop_next])
        s2 <- rowSums(A[1:nop,])
      }
      
      # convert link matrix to representation in the list format
      links <- which(A[1:nop,]==1,arr.ind=T)
      Ilinks <- links[,1]
      Jlinks <- links[,2]
      # if link is to dummy particle, set index to -1
      Jlinks[which(Jlinks==(nop_next+1))] <- -1
      
      # set links in the list object
      particleList[[m]]$nxt[Ilinks,n] <- Jlinks
    }
    # shrink curr_linkrange if needed at the end of the frames
    if(m==(frames_number-curr_linkrange-1) && curr_linkrange > 1)
      curr_linkrange <- curr_linkrange - 1
    
  }
  #   cat("I GOT HERE ----")
  # terminate all links at the list objects at the very last frame
  particleList[[frames_number]]$special[] <- FALSE
  particleList[[frames_number]]$nxt[,] <- -1  
  
  
  
  
  return(particleList)  
}


### dismissed!

linkParticles_v3_OLD_working_11.04.2014 <- function(particleList=imgObjectsList_v3,L,R=2,epsilon=0.1,lambda1=1,lambda2=1,nframes=length(candidate_feats_red))
{
  # preliminary step to set the particles next arrays (or matrix in our case) according to the linkrange
  # done now in case linkrange can get modified during the runs or across different runs
  cat("Starting up...\n\n")
  frames_number <- nframes
  linkrange <- R
  for (fr in 1:frames_number)
  {
    particleList[[fr]]$nxt <- matrix(0,nrow=nrow(particleList[[fr]]$particles),ncol=R)
  }
  
  curr_linkrange <- R
  displacement <- L
  # we need at least two frames
  if(frames_number < 2) 
  {
    # error
    cat("At least",R+1,"frames are needed!!! - INTERRUPTING!!!\n")
  }
  
  # if the linkrange is too big, set it to the right value
  if(frames_number < (curr_linkrange+1))
  {
    curr_linkrange <- frames_number - 1
  }
  
  # max cost = displacement^2
  for(m in 1:(frames_number - curr_linkrange + 1))
  {
    cat("\n---------- Linking frame",m,"of",frames_number,"...........\n")
    nop <- nrow(particleList[[m]]$particles)
    for(i in 1:nop)
    {
      particleList[[m]]$special[i] <- FALSE
      #       for(n in 1:linkrange)
      #       {
      #         particleList[[m]]$nxt[i,n] <- -1
      #       }
    }
    particleList[[m]]$nxt[,] <- -1
    
    
    # AND HERE BEGINS THE "TRUE" CYCLE THROUGHOUT NEXT FRAMES
    for(n in 1:curr_linkrange)
    {
      max_cost <- n*displacement*displacement
      
      nop_next <- nrow(particleList[[m+n]]$particles)
      
      
      
      
      # setup the cost matrix
      C <- n*displacement*displacement*matrix(1,nop+1,nop_next+1)
      
      ## PLUS VARIATION
      # quadratic distance between p_i and q_j
      xrep <- repmat(t(particleList[[m+n]]$particles[,1]),nop,1) # function repmat replicates the matlab behaviour
      yrep <- repmat(t(particleList[[m+n]]$particles[,2]),nop,1)
      xdiff <- xrep - repmat(particleList[[m]]$particles[,1],1,nop_next)
      ydiff <- yrep - repmat(particleList[[m]]$particles[,2],1,nop_next)
      deltaSquared <- xdiff^2 + ydiff^2
      
      alpha <- atan2(ydiff,xdiff)
      #       newCost <- lambda1 * (deltaSquared / (1-lambda2*(alpha/(pi+epsilon))))
      newCost <- deltaSquared
      # cost function for link p_i, q_j
      
      # for everything apart from the dummies, already set to rL^2...
      C[1:nop,1:nop_next] <- newCost # + squared moments and so on
      C[nop+1,nop_next+1] <- 0 # dummy to dummy      
      C[C>curr_linkrange*displacement*displacement] <- Inf
      
      
      
      
      # empty association matrix  
      A <- matrix(0,nrow=nop+1,ncol=nop_next+1)
      # initialize link matrix A --- oldstyle, but working ;)
      for(i in 1:nop)
      {
        # sort costs of real particles
        srtcst <- sort(C[i,])
        srtidx <- order(C[i,])
        # append index of dummy
        iidx <- 1
        dumidx <- which(srtidx==n+1)
        # search for available particle of smallest cost or dummy
        while(sum(A[,srtidx[iidx]])!=0 & (iidx < dumidx))
        {
          iidx <- iidx +1      
        }
        A[i,srtidx[iidx]] <- 1
      }
      
      # set dummy particle for columns with no entry
      s <- colSums(A) # or colSums here?!
      A[nop+1,which(s<1)] <- 1
      #   s <- colSums(A)
      #   A[which(s<1),n+1] <- 1
      # dummy corresponds to dummy
      A[nop+1,nop_next+1] <- 1
      # consistency checks
      s1 <- colSums(A[,1:nop_next])
      s2 <- rowSums(A[1:nop,])
      
      #       A
      #       #     A[1:15,1:15] <- diag(15) # try to perturb :)
      #       # iteration loop for the logistic transportation algorithm
      #       finished <- FALSE
      #       iterate <- 0
      #       mincost <- c()
      #     
      
      
      ############### weg! --------- clarify something with 0 or 1 based indexes....!!!
      # newstyle initialization
      # helper vector of boolean for initialization
      okv <- rep(TRUE,nop_next+1)
      # helper vectors - keep track of the empty columns
      g_x <- rep(0,nop_next+1)
      g_y <- rep(0,nop+1)
      g <- matrix(0,nrow=nop+1,ncol=nop_next+1)
      for(i in 1:nop) # looping over the x axis
      {
        min <- max_cost
        prev <- -1
        for(j in 1:nop_next) # loop over y axis without the dummy
        {
          # let's see if we can use this coordinate
          if(okv[j] && (min>C[i,j]))
          {
            min <- C[i,j]
            if(prev>=0)
            {
              okv[prev] <- TRUE
              g[i,prev] <- FALSE
            }
            okv[j] <- FALSE
            g[i,j] <- TRUE
            prev <- j
          }
        }
        # check if we have a dummy particle
        if(min == max_cost)
        {
          if(prev >=0)
          {
            okv[prev] <- TRUE
            g[i,prev] <- FALSE
          }
          g[i,nop_next] <- TRUE
          okv[nop_next] <- FALSE
        }
      }
      # look for columns that are zero
      for(j in 1:nop_next)
      {
        ok <- 1
        for(i in 1:nop+1)
        {
          if(g[i,j])
            ok <- 0
        }
        if(ok == 1)
          g[nop,j] <- TRUE
      }
      # build g_x and g_y, speedups for g
      for(i in 1:nop+1)
      {
        for(j in 1:nop_next+1)
        {
          if(g[i,j])
          {
            g_x[j] <- i
            g_y[i] <- j
          }
        }
      }
      g_x[nop_next+1] <- nop+1
      g_y[nop+1] <- nop_next+1
      ###############    ------   weg!
      
      
      
      # optimize the relation matrix
      finished <- 0
      iterate <- 0
      mincost <- c()
      while(!finished)
      {
        iterate <- iterate + 1
        cat("Iteration",iterate,".....")
        # non-set links of finite costs
        todo <- intersect(which(A[1:nop,1:nop_next]==0),which(C[1:nop,1:nop_next]<Inf) )
        
        Icand = ((todo-1) %% nop) + 1
        Jcand = floor((todo-1) / nop) + 1
        
        # determine the reduced cost Cred for each candidate insertion
        # initialize
        Cred <- rep(0,length(todo))
        Xcand <- rep(0,length(todo))
        Ycand <- rep(0,length(todo))
        # compute iteratively
        for(ic in 1:length(Icand))
        {
          Xcand[ic] <- which(A[Icand[ic],]==1)
          Ycand[ic] <- which(A[,Jcand[ic]]==1)
          Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        }
        
        # find minimum cost of corresponding action
        minc <- sort(Cred)[1]
        mini <- order(Cred)[1]
        mincost <- c(mincost,minc)
        
        # if minimum < 0, link addition is favorable
        if(minc < 0)
        {
          cat("Performing change!")
          # add link and update dependencies to preserve the topology
          A[Icand[mini],Jcand[mini]] <- 1
          A[Ycand[mini],Jcand[mini]] <- 0
          A[Icand[mini],Xcand[mini]] <- 0
          A[Ycand[mini],Xcand[mini]] <- 1
          
        } else {
          # the best is already done and there is no room for improving the cost function
          finished <- TRUE
        }
        
        # consistency checks--..
        s1 <- colSums(A[,1:nop_next])
        s2 <- rowSums(A[1:nop,])
      }
      
      # convert link matrix to representation in the list format
      links <- which(A[1:nop,]==1,arr.ind=T)
      Ilinks <- links[,1]
      Jlinks <- links[,2]
      # if link is to dummy particle, set index to -1
      Jlinks[which(Jlinks==(nop_next+1))] <- -1
      
      # set links in the list object
      particleList[[m]]$nxt[Ilinks,n] <- Jlinks
    }
    if(m==(frames_number-curr_linkrange-1) && curr_linkrange > 1)
      curr_linkrange <- curr_linkrange - 1
    
  }
  cat("I GOT HERE ----")
  # terminate all links at the list objects at the very last frame
  particleList[[frames_number]]$special[] <- FALSE
  particleList[[frames_number]]$nxt[,] <- -1  
  
  
  
  
  return(particleList)  
}
