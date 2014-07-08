# assistant to development

library("flowcatchR")

imgRepository <- list.files("/Volumes/users$/marinif/flow/test_28_03_2014/cutout2/",full.names=T,pattern="*.tif")
frameList1 <- newFrameList(nframes=100,imgsLocation=imgRepository)

ffffg <- cut(x=frameList1,cutAll=20)

cc <- createChannelsFrameList(frameList1)

prepro <- preprocess.ChannelsFrameList(cc,"red")
framelistProcessed <- prepro
framelistRaw <- cc[[1]]

papa <- extractParticles(framelistRaw,framelistProcessed)

qaqa <- filterParticles(particlelist=papa)
# rara <- initialize.ParticleList(qaqa)

sasa <- link.ParticleList(qaqa,L=26,R=3,epsilon1=0,epsilon2=0,lambda1=1,lambda2=0,nframes=100,useAreaChange=FALSE)
tata <- generate.TrajectoryList(sasa)


rere <- paintTrajectory(tata,frameList1,framelistProcessed,trajId=3)
fullInspection.FrameList(rere)
segseg <- combine.preprocessedFrameList(frameList1,framelistProcessed) # if framelist raw is with colors, then also the output is..
inspect.FrameList(framelist=segseg)

segcsegc <- combineWcolor.preprocessedFrameList(frameList1,framelistProcessed)
inspect.FrameList(framelist=segcsegc)

display.TrajectoryList(tata,qaqa)


tataMOD <- evaluateTrajectoryList(tata,frameList1,framelistProcessed)
unlist(lapply(tataMOD,function(arg){arg$keep}))

# [1]    NA  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
# [31] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [91]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE    NA FALSE
# [121] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

# wawa <- extractKinematics.TrajectoryList(tata)
# 
# extractKinematics.traj(tata,1)‰
# 
# extractKinematics.traj(tata,13)
# plot(extractKinematics.traj(tata,13)$trajMSD)
# rere2 <- paintTrajectory(tata,frameList1,framelistProcessed,trajId=13)
# fullInspection.FrameList(rere2)

# with the new code structure
wawa2 <- extractKinematics(tata)

extractKinematics.traj(tata,1)

extractKinematics.traj(tata,13)
plot(extractKinematics.traj(tata,13)$trajMSD)
rere2 <- paintTrajectory(tata,frameList1,framelistProcessed,trajId=13)
fullInspection.FrameList(rere2)

singleKFS <- extractKinematics(tata,13)
singleK_manyTrajs <- extractKinematics(tata,feature="curvilinearVelocity")
singleKF <- extractKinematics(tata,trajectoryID=13,feature="curvilinearVelocity")

#' @title
#' @description
#' @details
#'  
#' @param
#' @param
#' 
#' @return
#' 
#' @keywords
#' @seealso
#' @references
#' 
#' @examples
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014














# learning S3...
infant <- function(ID,sex,age,ht,wt)
{
  out <- list(ID=ID,sex=sex,data=data.frame(Age=age,HT.cm=ht,WT.kg=wt))
  class(out) <- "infant"
  invisible(out)
}

print.infant <- function(object)
{
  cat("ID =",object$ID,"\nSex =",object$sex,"\n")
  print(object$data)
}

# Plot method for infant class
plot.infant <- function(object) {
  data <- object$data
  par(mfrow=c(1,2))
  plot(data$Age, data$HT.cm, type="o", pch=19, col="blue", xlab="Age (months)", ylab="Height (cm)", main="Height vs Age")
  plot(data$Age, data$WT.kg, type="o", pch=19, col="blue", xlab="Age (months)", ylab="Weight (kg)", main="Weight vs Age")
  mtext(paste("Subject ",object$ID,", ",toupper(object$sex),sep=""), side=3,outer=TRUE, line=-1.5, cex=1.5)
}



age       = c(0, 3, 6, 12, 18, 24, 30, 36)
male.wt   = c( 3.53,  6.39,  8.16, 10.46, 11.80, 12.74, 13.56, 14.33)
female.wt = c( 3.40,  5.86,  7.45,  9.67, 11.09, 12.13, 13.04, 13.87)
male.ht   = c(49.99, 62.08, 67.86, 76.11, 82.41, 87.66, 92.13, 95.45)
female.ht = c(49.29, 60.46, 66.12, 74.40, 80.80, 86.20, 91.13, 94.43)
# Create infant objects
x <- infant(1, "male", age, male.ht, male.wt)
y <- infant(2, "female", age, female.ht, female.wt)
class(x); class(y)
# Print infant objects
x; y
# Plot infant objects
plot(x); plot(y)
# Possible to create invalid objects with S3 classes
z <- infant(3, "male", c("0 mon", "3 mon"), c(49.99,62.08), c(3.53,6.39))


