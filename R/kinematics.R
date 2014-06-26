## working on a TrajectoryList object...


extractKinematics.traj <- function(trajectorylist,
                                   trajectoryID,
                                   acquisitionFrequency=30, # in milliseconds
                                   scala=50 # 1 pixel is ... micrometer
                                   )
{
  singleTraj <- trajectorylist[[trajectoryID]]$trajectory
  # throw a warning/message/error if the traj is below 3 points?
  if(nrow(singleTraj) < 4)
  {
    warning("The trajectory had 3 or less points, no features were computed!")
    out <- list(notEmpty=FALSE)
    class(out) <- c("KinematicsFeatureSet",class(out))
    return(out)
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
  
  linearityForwardProgression <- curvilinearVelocity / straightLineVelocity
  
  # msd only to compute where no gaps are present?
  trajMSD <- computeMSD(sx,sy,until=floor(nrow(singleTraj)/4))
  velocityAutoCorr <- acf(delta.v,plot=FALSE)
  # 
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
              notEmpty=TRUE
              )
  class(out) <- c("KinematicsFeatureSet",class(out))
  return(out)
}



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



toPolarCoords <- function(x,y)
{
  Theta <- atan2(y,x)
  Radius <- sqrt(x^2 + y^2)
  return(list(Theta=Theta,Radius=Radius))
}


toCartesianCoords <- function(Theta,Radius)
{
  xCoord <- Radius * cos(Theta)
  yCoord <- Radius * sin(Theta)
  return(list(x=xCoord,y=yCoord))
}



extractKinematics.TrajectoryList <- function(trajectorylist,
                                             ...) # for frame rate and spatial scala
{
  out <- list()
  for(i in 1:length(trajectorylist))
  {
    kineOne <- extractKinematics.traj(trajectorylist,i) #, acquisitionFrequency=30, # in milliseconds, scala=50
    out[[length(out) + 1]] <- kineOne
  }
  # additional class attribute?
  class(out) <- c("KinematicsFeatureSetList",class(out))
  return(out)
}



velohist <- function(kinematicsfeaturesetlist)
{
  curvilinearVelocities <- unlist(lapply(kinematicsfeaturesetlist,function(arg){arg$curvilinearVelocity}))
  hist(curvilinearVelocities,breaks=20,probability=TRUE)
  lines(density(curvilinearVelocities),col="steelblue")
}



distanceDiagnostics <- function()
{
  cat("something computing distances intra-particles- raises a flag/warning if distances (normalized) are less than 1.5 radius or so...")
}

# 
# 
# TRAJECTORY ANALYSIS
# The final stage in any time-lapse microscopy imaging experiment is the analysis of the trajectories re- sulting from cell or particle tracking, to confirm or reject predefined 
# hypotheses about object dynamics, or to discover new phenomena. Qualitative analysis by visual inspection of computed trajectories may already give hints about trends in the data,
# but usually does not provide much more information than can be obtained by directly looking at the image data itself, or projections thereof. Quantitative anal- yses of the 
# trajectories are required in order to achieve higher sensitivity in data interpretation and to be able to perform statistical tests. Of course, what parameters to measure and 
# analyze depends very much on the research questions underlying a specific experiment. Here we briefly discuss examples of meas- urements frequently encountered in the literature.
# 
# Geometry Measurements
# Once the objects of interest in an image sequence are detected, segmented, and associated, a multitude of measures concerning the geometry of the resulting trajectories as well
# as the objects themselves can readily be computed. An example is the maximum relative distance to the initial position reached by the object [27,28,57]. Other examples are the
# length of the trajectory, or the total distance traveled by the object, and the distance between start and end point, or the net distance traveled [12]. The latter measures 
# relate to the so-called McCutcheon index [32,91], which is often used in chemotaxis studies to quantify the efficiency of cell movements, and is defined as the ratio between 
# the net distance moved in the direction of increasing chemoattractant concentration and the total distance moved. Derived pa- rameters, such as the directional change per 
# time interval and its autocorrelation [129,154], are indica- tive of the directional persistence and memory of a translocating cell. Information about the cell con- tour 
# or surface at each time point allow the computation of a variety of shape features, such as diame- ter, perimeter and area, or surface area and volume, circularity or 
# sphericity, convexity or concavity [129], elongation or dispersion [33], and their changes over time.
# 
# Diffusivity Measurements
# A very frequently studied parameter, especially in particle tracking experiments, is the mean square displacement (MSD) [4,6,16,35,54,72,74,78,89,115]. It is a convenient
# measure to study the diffusion characteristics of the motion of individual particles [104,119,134] and also allows to assess the viscoe- lastic properties of the media in
# which they move [140,141,134]. By definition, the MSD is a function of time lag, and the shape of the MSD-time curve for a given trajectory is indicative of the mode of 
# motion of the corresponding particle (Figure 4). For example, in the case of pure or normal diffusion by thermally driven Brownian motion, the MSD will increase linearly 
# as a function of time, where the dif- fusion constant determines the slope of the line. In the case of flow or active transport, on the other hand, the MSD will increase 
# more rapidly and in a nonlinear fashion. The contrary case of anomalous subdiffusion, characterized by a lagging MSD-time curve compared to normal diffusion, occurs if the
# motion is not free but impeded by obstacles. Confined motion, caused by corrals or tethering or other restrictions, manifests itself by a converging curve, where the 
# limiting MSD value is proportional to the size of the region accessible for diffusion. Mathematically, the MSD is the second-order moment of displacement. A more complete
# characterization of a diffusion process is obtained by computing all moments of displacement up to some order [43,120].
# Some prudence is called for in diffusivity measurements. In isotropic media, where the displacements in each of the three spatial dimensions may be assumed to be 
# uncorrelated, the 2D diffusion coefficient is equal to the 3D diffusion coefficient [134]. In practical situations, however, it may be unknown a priori whether isotropy 
# can be assumed. In this context we recall our warning remarks regarding 2D versus 3D motion analysis [35,152,166] and stress again the importance of experimental verification
# of one’s assumptions. Furthermore, the diffusivity of a particle may depend on the diameter of the particle compared to the microstructure of the biological fluid in 
# which it moves. Here, a distinction must be made between microscopic, mesoscopic, and macroscopic diffusion [134]. Also, in the case of normal diffusion, the relation 
# between the slope of the MSD-time line and the diffusion constant strictly holds only for infinite trajectories [104]. The shorter the trajectories, the larger the 
# statistical fluctuations in the diffusivity measurements, and the higher the relevance of studying distributions of diffusion con- stants rather than single values. 
# But even for very long trajectories, apparent subdiffusion patterns may arise at short time scales, caused solely by the uncertainty in particle localization in noisy
# images [89]. Finally, care must be taken in computing the MSD over an entire trajectory, as it may obscure transi- tions between diffusive and nondiffusive parts [119].
# 
# Velocity Measurements
# Another commonly studied parameter in time-lapse imaging experiments is velocity [10,29,32,57,90, 127,153]. It is computed simply as distance over time. Instantaneous 
# object velocity can be estimated as the distance traveled from one frame to the next divided by the time interval. Average velocity, also referred to as curvilinear 
# velocity, is then computed as the sum of the frame-to-frame distances trav- eled, divided by the total time elapsed. If the temporal sampling rate is constant, this
# is the same as av- eraging the instantaneous velocities. The so-called straight-line velocity, another type of average veloc- ity, is computed as the distance between
# the first and last trajectory position divided by the total elapsed time. The ratio between the latter and the former, known as the linearity of forward progression 
# [70,84,162], is reminiscent of the McCutcheon index mentioned above. Histograms of velocity [10,32, 88,104,144,145,146] are often helpful in gaining insight into motion
# statistics. Object acceleration can also be estimated from velocity but is rarely studied [129].
# 
# Several warning remarks are in order regarding velocity estimation. In the case of cell tracking, motion analysis is tricky, due to the possibility of morphological
# changes over time. Often, to circumvent the problem, a center position is tracked [25,29,45,63,70,83,84,153,162]. In the case of highly plastic cells, however, 
# centroid based velocity measurements can be very deceptive [129]. For example, an anchored cell may extend and retract pseudopods, thereby continuously changing
# its centroid position and gener- ating significant centroid velocity, while the cell is not actually translocating. In the contrary case, a cell may spread in 
# all directions at high velocity in response to some stimulant, while the cell centroid position remains unchanged. Another warning concerns the accuracy of velocity
# estimation in relation to the temporal sampling rate [129]. The higher this rate, the more detailed the movements are cap- tured, and the closer the velocity 
# estimates will approach the true values. Statistically speaking, as the sampling rate decreases, velocities will on average be increasingly underestimated.
# 
