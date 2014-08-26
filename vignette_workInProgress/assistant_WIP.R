# assistant to development

library("flowcatchR")

imgRepository <- list.files("/Volumes/users$/marinif/flow/test_28_03_2014/cutout2/",full.names=T,pattern="*.tif")
frameList1 <- read.frames(image.files = imgRepository,nframes=100)

ffffg <- cut(x=frameList1,cutAll=20)

cc <- channels(frameList1)

prepro <- preprocess.ChannelsFrameList(cc,"red")
framelistProcessed <- prepro
framelistRaw <- cc[[1]]

papa <- particles(framelistRaw,framelistProcessed)

qaqa <- select.particles(particlelist=papa)
# rara <- initialize.ParticleList(qaqa)
sum(unlist(lapply(qaqa,function(arg){nrow(arg$particles)})))
# [1] 1749

sasa <- link.particles(qaqa,L=26,R=3,epsilon1=0,epsilon2=0,lambda1=1,lambda2=0,nframes=100,include.area=FALSE)
tata <- trajectories(sasa)


rere <- add.contours(frameList1,framelistProcessed,tata,trajId=3)
inspect.frames(rere)
inspect.frames(rere,99,inspectAll=TRUE)

rere2 <- add.contours2(frameList1,framelistProcessed,tata,trajId=3,mode="trajectories")

rere3 <- add.contours2(frameList1,framelistProcessed,tata,trajId=c(3,4,5,6),mode="trajectories")
rere4 <- add.contours2(frameList1,framelistProcessed,tata,mode="trajectories")
export.frames(rere4,nameStub=paste0("new_evaluation_trajAll_"),createGif=TRUE,removeAfterCreatingGif=TRUE)


# segseg <- combine.preprocessedFrameList(frameList1,framelistProcessed) # if framelist raw is with colors, then also the output is..
# inspect.FrameList(framelist=segseg)

# segcsegc <- combineWcolor.preprocessedFrameList(frameList1,framelistProcessed)
# inspect.FrameList(framelist=segcsegc)




plot(tata,qaqa)


tataMOD <- evaluateTrajectoryList(tata,frameList1,framelistProcessed)
unlist(lapply(tataMOD,function(arg){arg$keep}))

# [1]    NA  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE
# [31] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# [91]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE    NA FALSE
# [121] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE


# export.frames(paintedTraj,nameStub=paste0("evaluation_traj_AllTrajectories"),createGif=TRUE,removeAfterCreatingGif=TRUE)
# 
# # "interactive" part, asking the user whether the trajectory is correct
# 
# 
# # best thing, prompt for something like "did you like the trajectory? :)"
# #   interactive() <- TRUE # does not work-..
# #   if(interactive()==FALSE)  userInput <- readline(prompt="Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
# 
# 
# cat("Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
# userInput <- readLines(n = 1L)
# #   userInput <- readline(prompt="Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
# # if no 0 nor 1, error/do not update, reprompt?
# # otherwise, this becomes the value for the field
# # ... else
# out[[id]]$keep <- as.logical(as.numeric(userInput))

tataEval <- tata
for(i in 1:length(tata))
{
  paintedTraj <- inspect.trajectories(tata,frameList1,framelistProcessed,i,F)
  export.frames(paintedTraj,nameStub=paste0("evaluation_traj_oneByOne_",i),createGif=TRUE,removeAfterCreatingGif=TRUE)
  # take a sec to evaluate
  # and eventually update the slot?
  cat("Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
  userInput <- readLines(n = 1L)
  # if no 0 nor 1, error/do not update, reprompt?
  # otherwise, this becomes the value for the field
  tataEval[[i]]$keep <- as.logical(as.numeric(userInput))
  
}




# with the new code structure
wawa2 <- kinematics(tata)

extractKinematics.traj(tata,1)

extractKinematics.traj(tata,13)
plot(extractKinematics.traj(tata,13)$trajMSD)
rere2 <- add.contours(frameList1,framelistProcessed,tata,trajId=13)
inspect.frames(rere2)

singleKFS <- extractKinematics(tata,13)
singleK_manyTrajs <- extractKinematics(tata,feature="curvilinearVelocity")
singleKF <- extractKinematics(tata,trajectoryID=13,feature="curvilinearVelocity")





## testing out on transmigration

library("flowcatchR")
imgRepository <- list.files("/Volumes/users$/marinif/flow/Transmigration/RawImages/serie1/",full.names=T,pattern="*.jpeg")
imgRepository
frameListTrans <- read.frames(image.files = imgRepository,nframes=14)

preprocess(frameListTrans)
preproTrans <- preprocess(frameListTrans)
partTrans <- particles(frameListTrans,preproTrans)

linkTrans <- link.particles(partTrans,L=26,R=2,epsilon1=0,epsilon2=0,lambda1=1,lambda2=0,nframes=14,include.area=FALSE)
trajTrans <- trajectories(linkTrans)










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
#' load(file.path(system.file("extra", package="flowcatchR"),"MesenteriumSubset.RData"))
#' plateletsFrameList <- channels(MesenteriumSubset)$red
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


