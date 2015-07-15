#' Links a \code{ParticleSet} object
#' 
#' Performs linking of the particles by tracking them through the frames
#'  
#' 
#' @param particleset A \code{ParticleSet} object
#' @param L Maximum number of pixels an object can move in two consecutive frames
#' @param R Linkrange, i.e. the number of consecutive frames to search for potential candidate links
#' @param epsilon1 A numeric value, to be used in the formula. Jitter for allowing angular displacements
#' @param epsilon2 A numeric value, to be used in the formula. Jitter for allowing spatial displacements
#' @param lambda1 A numeric value. Multiplicative factor for the penalty function
#' @param lambda2 A numeric value. Multiplicative factor applied to the angular displacement
#' @param penaltyFunction A function structured in such a way to be applied as penalty function in the linking
#' @param verboseOutput Logical, whether the output should report additional intermediate steps. For debugging use mainly
#' @param prog Logical, whether the a progress bar should be shown during the tracking phase
#' @param include.intensity Logical, whether to include also intensity change of the particles in the cost function calculation
#' @param include.area Logical, whether to include also area change of the particles in the cost function calculation
#' 
#' @references I F Sbalzarini and P Koumoutsakos."Feature point tracking and trajectory analysis for video imaging in cell biology."
#' In: Journal of structural biology 151.2 (Aug. 2005), pp. 182-95. ISSN: 1047-8477. DOI: 10.1016/j.jsb.2005.06.002.
#' URL: http://www.ncbi.nlm.nih.gov/pubmed/16043363
#' 
#' @return A \code{LinkedParticleSet} object
#' 
#' @examples
#' data("candidate.platelets")
#' tracked.platelets <- link.particles(candidate.platelets, L= 40)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
link.particles <- function(particleset,
                           L,
                           R=2,
                           epsilon1=0.1,
                           epsilon2=2,
                           lambda1=1,
                           lambda2=1,
                           penaltyFunction=penaltyFunctionGenerator(),
                           verboseOutput=FALSE,
                           prog=FALSE,
                           include.intensity=TRUE,
                           include.area=TRUE)
{
  out <- initialize.LinkedParticleSet(particleset)
  if (prog) pb <- txtProgressBar(min = 0, max = 100, style = 3)
  # preliminary step to set the particles next arrays (or matrix in our case) according to the linkrange
  # done now in case linkrange can get modified during the runs or across different runs
  if (verboseOutput) cat("Starting up...\n\n")
  frames_number <- length(out)
  for (fr in 1:frames_number)
  {
    out@tracking[[fr]]$nxt <- matrix(0,nrow=nrow(out@.Data[[fr]]),ncol=R)
  }
  curr_linkrange <- R # as it can be updated afterwards
  displacement <- L
  # we need at least two frames
  if(frames_number < 2) {
    stop("At least", R + 1,"frames are needed! - Interrupting the tracking...\n")
  }
  
  # if the linkrange is too big, set it to the right value
  if (frames_number < (curr_linkrange + 1)) {
    curr_linkrange <- frames_number - 1
  }
  
  for (m in 1:(frames_number - 1)) {
    if (verboseOutput) cat("---------- Linking frame",m,"of",frames_number,"...........\n")
    nop <- nrow(out@.Data[[m]])
    out@tracking[[m]]$special[] <- FALSE
    out@tracking[[m]]$nxt[,] <- -1
    
    # AND HERE BEGINS THE "TRUE" CYCLE THROUGHOUT NEXT FRAMES
    iterate <- 0
    for (n in 1:curr_linkrange)
    {
      iterate <- 0
      max_cost <- n * displacement * displacement    
      nop_next <- nrow(out@.Data[[m + n]])
      ## cost matrix
      # setup the cost matrix
      C <- n * displacement * displacement * matrix(1,nop + 1,nop_next + 1)
      ## PLUS VARIATION
      # quadratic distance between p_i and q_j
      xrep <- repmat(t(out@.Data[[m + n]][,1]),nop,1) # function repmat replicates the matlab behaviour
      yrep <- repmat(t(out@.Data[[m + n]][,2]),nop,1)
      xdiff <- xrep - repmat(out@.Data[[m]][,1],1,nop_next)
      ydiff <- yrep - repmat(out@.Data[[m]][,2],1,nop_next)
      deltaSquared <- xdiff^2 + ydiff^2
      alpha <- atan2(ydiff,xdiff)
      
      deltaSquared <- (deltaSquared - epsilon2) * ((deltaSquared - epsilon2) > 0)
      # this allows to have a mini jitter on the position of the particle, that can now go slightly backwards and or move orthogonally to the flow
      
      ## similarly, include the part where the area/intensity is considered
      areaVariation <- 0
      # using the ... 6th column, corresponding to the area
      areaDiff <- repmat(t(out@.Data[[m + n]][,"cell.0.s.area"]),nop,1) - repmat(out@.Data[[m]][,"cell.0.s.area"],1,nop_next)
      areaVariation <- areaDiff^2
      
      intensityVariation <- 0
      # using the ... corresponding column, corresponding to the mean intensity
      intDiff <- repmat(t(out@.Data[[m + n]][,"cell.a.b.mean"]),nop,1) - repmat(out@.Data[[m]][,"cell.a.b.mean"],1,nop_next)
      intensityVariation <- intDiff^2
      
      #       distFunction <- deltaSquared + ifelse(include.area,areaVariation,0) + ifelse(include.intensity,intensityVariation,0)
      distFunction <- deltaSquared
      if (include.intensity) distFunction <- distFunction + intensityVariation
      if (include.area) distFunction <- distFunction + areaVariation

      newCost <- penaltyFunction(alpha,distFunction)
      
      #       newCost <- deltaSquared
      # cost function for link p_i, q_j
      # for everything apart from the dummies, already set to rL^2...
      C[1:nop,1:nop_next] <- newCost # + squared moments and so on
      C[nop + 1,nop_next + 1] <- 0 # dummy to dummy
      C[C > max_cost] <- Inf
      
      ## association matrix
      # empty association matrix
      A <- matrix(0,nrow=nop + 1,ncol=nop_next + 1)
      # initialize link matrix A --- oldstyle, but working ;) and probably even more optimized than the for for for loops
      for (i in 1:nop) {
        # sort costs of real particles
        # srtcst <- sort(C[i,])
        srtidx <- order(C[i,])
        # append index of dummy
        iidx <- 1
        dumidx <- which(srtidx == (nop_next + 1))
        # search for available particle of smallest cost or dummy
        while(sum(A[,srtidx[iidx]]) != 0 && (iidx < dumidx)) {
          iidx <- iidx +1
        }
        A[i,srtidx[iidx]] <- 1
      }
      # set dummy particle for columns with no entry
      s <- colSums(A) 
      A[nop + 1,which(s < 1)] <- 1
      # dummy corresponds to dummy
      A[nop + 1,nop_next + 1] <- 1
      # consistency checks
      s1 <- colSums(A[,1:nop_next])
      s2 <- rowSums(A[1:nop,])
      # optimize the relation matrix
      finished <- 0
      mincost <- c()
      while(!finished) {
        iterate <- iterate + 1
        if (verboseOutput) cat("Frame:",m,"\t---\tLink_range:",n,"\t---\tIteration",iterate,".....")
        # non-set links of finite costs
        todo <- intersect(which(A[1:nop,1:nop_next] == 0),which(C[1:nop,1:nop_next] < Inf) )
        Icand <- ((todo - 1) %% nop) + 1
        Jcand <- floor((todo - 1) / nop) + 1
        # determine the reduced cost Cred for each candidate insertion
        # initialize
        
        Cred <- rep(0,length(todo))
        Xcand <- rep(0,length(todo))
        Ycand <- rep(0,length(todo))
        # compute iteratively
        for (ic in 1:length(Icand)) {
          Xcand[ic] <- which(A[Icand[ic],] == 1)
          Ycand[ic] <- which(A[,Jcand[ic]] == 1)
          Cred[ic] <- C[Icand[ic],Jcand[ic]] + C[Ycand[ic],Xcand[ic]] - C[Icand[ic],Xcand[ic]] - C[Ycand[ic],Jcand[ic]]
        }
        
        # find minimum cost of corresponding action
        minc <- sort(Cred)[1]
        mini <- order(Cred)[1]
        mincost <- c(mincost,minc)
        
        # if minimum < 0, link addition is favorable
        if (!is.na(minc) && (minc < 0)) {
          if (verboseOutput) cat("--> Performing change.\n")
          # add link and update dependencies to preserve the topology
          A[Icand[mini],Jcand[mini]] <- 1
          A[Ycand[mini],Jcand[mini]] <- 0
          A[Icand[mini],Xcand[mini]] <- 0
          A[Ycand[mini],Xcand[mini]] <- 1
          
        } else {
          # the best is already done and there is no room for improving the cost function
          finished <- TRUE
          if(verboseOutput) cat("--> Found optimal solution for the current cost function!\n")
        }
        
        # consistency checks--..
        s1 <- colSums(A[,1:nop_next])
        s2 <- rowSums(A[1:nop,])
      }
      
      # convert link matrix to representation in the list format
      links <- which(A[1:nop,] == 1,arr.ind=TRUE)
      Ilinks <- links[,1]
      Jlinks <- links[,2]
      # if link is to dummy particle, set index to -1
      Jlinks[which(Jlinks == (nop_next + 1))] <- -1
      
      # set links in the list object
      out@tracking[[m]]$nxt[Ilinks,n] <- Jlinks
    }
    # shrink curr_linkrange if needed at the end of the frames
    if(m == (frames_number - curr_linkrange) && curr_linkrange > 1) {
      curr_linkrange <- curr_linkrange - 1
    }
    if (prog) setTxtProgressBar(pb,(m / (frames_number - curr_linkrange + 1) * 100))
  }
  # terminate all links at the list objects at the very last frame
  out@tracking[[frames_number]]$special[] <- FALSE
  out@tracking[[frames_number]]$nxt[,] <- -1 
  if (prog) {
    setTxtProgressBar(pb, 100)
    close(pb)
  }
  return(out)
}




#' Generate a penalty function
#' 
#' A function to generate penalty functions to use while linking particles
#' 
#' @param epsilon1 A numeric value, to be used in the formula. Jitter for allowing angular displacements
#' @param epsilon2 A numeric value, to be used in the formula. Jitter for allowing spatial displacements
#' @param lambda1 A numeric value. Multiplicative factor for the penalty function
#' @param lambda2 A numeric value. Multiplicative factor applied to the angular displacement
#' 
#' @return A function object, to be used as penalty function
#' 
#' @examples
#' custom.function <- penaltyFunctionGenerator(epsilon1=0.1,epsilon2=6,lambda1=1.5,lambda2=0)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
penaltyFunctionGenerator <- function(epsilon1=0.1,
                                     epsilon2=2,
                                     lambda1=1,
                                     lambda2=1)
{


  # TODO:
  # request that lambda2 is at most 1 to maintain meaningful penalty shapes
  
  # something like stop if lambda2 > 1?
  
  # this function returns a function that is adopted afterwards in the tracking
  # it can be defined by default as we coded it, or the user can actually "invent" one of his taste 
  function(angle,distance) { 
    lambda1 * ( distance / (1 - lambda2 * ( abs(angle) / (pi + epsilon1) )))

  }
}
















#' Initialize a \code{ParticleSet} object for subsequent linking/tracking
#'  
#' @param particleset A \code{ParticleSet} object
#' @param linkrange The number of frames to look for candidate particles potentially belonging to the same track
#' 
#' @return A \code{ParticleSet} object with slots dedicated for the tracking pre-filled
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
initialize.LinkedParticleSet <- function(particleset,
                                         linkrange=1)   
{
  out <- new("LinkedParticleSet",
             .Data = particleset@.Data,
             channel = particleset@channel)
  # for the tracking slot...
  tmp <- vector(length(particleset),mode="list")
  for (i in 1:length(particleset)) {
    particleNr <- nrow(particleset[[i]])
    tmp[[i]]$link <- rep(0,particleNr)
    tmp[[i]]$frame <- rep(i,particleNr)
    tmp[[i]]$label <- rep(NA,particleNr)
    tmp[[i]]$special <- rep(TRUE,particleNr)
    tmp[[i]]$nxt <- matrix(0,nrow=particleNr,ncol=linkrange)
  }
  out@tracking <- tmp
  return(out)
}





#' Function equivalent for MATLAB's repmat - Replicate and tile arrays
#' 
#' A more flexible and stylish alternative to replicate the behaviour of the repmat function of MATLAB
#' 
#' @param a The matrix to copy
#' @param n The n value for the tiling
#' @param m The m value for the tiling
#' 
#' @return Creates a large matrix consisting of an m-by-n tiling of copies of a. 
#' 
#' @references http://cran.r-project.org/doc/contrib/R-and-octave.txt
#' 
#' @author Robin Hankin, 2001
repmat <- function(a,n,m)
{
  kronecker(matrix(1,n,m),a)
}
# refer to "translation document matlab to R" for credits ;)

