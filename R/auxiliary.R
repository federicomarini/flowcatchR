showMe <- function(imgObject, dispMet="raster",...)
{
  display(imgObject, method=dispMet)
}




setupFolders <- function(projectFolder="~",analysisFolder)
{
  setwd(projectFolder)
  system(paste("mkdir ", analysisFolder,sep=""))
  processingFolder <- paste(projectFolder,analysisFolder,sep="")
  setwd(processingFolder)
  system("mkdir segmentedImages_red")
  system("mkdir segmentedImages_green")
  system("mkdir processingOverviews")
  system("mkdir thresholdOverviews")
  system("mkdir featuresReports_red")
  system("mkdir featuresReports_green")
  system("mkdir paintedObjects_red")
  system("mkdir paintedObjects_green")
  system("mkdir paintedCells_red")
  system("mkdir paintedCells_green")  
  system("mkdir postprocessing_red")  
  system("mkdir postprocessing_green")
  system("mkdir processedReports_green")
  system("mkdir processedReports_red")
  system("mkdir processedReportsForMOSAIC_green")
  system("mkdir processedReportsForMOSAIC_red")
  system("mkdir backgroundImages")
  system("mkdir cutoutImages")
  system("mkdir rotatedImages")
  
  system("mkdir backgroundsubtractedImages_allchannels")
  system("mkdir backgroundsubtractedImages_red")
  system("mkdir backgroundsubtractedImages_green")
  
  
  cat("Created folders for analysis in",processingFolder,"\n")
  return(processingFolder)
}

setupFoldersTransmigrating <- function(analysisFolder, projectFolder="~")
{
  setwd(projectFolder)
  system(paste("mkdir ", analysisFolder,sep=""))
  processingFolder <- paste(projectFolder,analysisFolder,sep="")
  setwd(processingFolder)
  system("mkdir segmentedImages")
  system("mkdir processingOverviews")
  system("mkdir thresholdOverviews")
  system("mkdir featuresReports")
  system("mkdir paintedObjects")
  system("mkdir paintedCells")  
  system("mkdir postprocessing")
  system("mkdir processedReports")
  
  cat("Created folders for analysis in",processingFolder)
  return(processingFolder)
}


################
calcDist <- function(frame1_obj,frame2_obj,method="euclidean")
{
  eudi <- sqrt((frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)^2 + (frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)^2)
  distx <- abs(frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)
  disty <- abs(frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)
  
  
  if(method=="euclidean")
    return(eudi)
  if(method=="dx")
    return(distx)
  if(method=="dy")
    return(disty)
}





calcDist2 <- function(frame1_obj,frame2_obj,movementDirection="leftToRight")
{
  eudi <- sqrt((frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)^2 + (frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)^2)
  distx <- abs(frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)
  disty <- abs(frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)
  
  if(movementDirection=="leftToRight")
    backDisp <- (frame1_obj$cell.0.m.cx - frame2_obj$cell.0.m.cx)
  if(movementDirection=="rightToLeft")
    backDisp <- (frame2_obj$cell.0.m.cx - frame1_obj$cell.0.m.cx) 
  if(movementDirection=="upToDown")
    backDisp <- (frame1_obj$cell.0.m.cy - frame2_obj$cell.0.m.cy)
  if(movementDirection=="downToUp")
    backDisp <- (frame2_obj$cell.0.m.cy - frame1_obj$cell.0.m.cy)
  
  
  res <- list(eudi,distx,disty,backDisp)
  names(res) <- c("euclideanDistance","xDisplacement","yDisplacement","backwardsDisplacement")
  return(res)
  
}
#################
repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}

################

################
listImages <- function(repositoryFolder,fullPattern=T,searchPattern="*tif")
{
  list.files(path=repositoryFolder, pattern=searchPattern,full.names=fullPattern)
}


################
# points <- locator(2,type="l",col="white",)

estimateAngle <- function()
{
  # to be called on a clear image
  pstart <- locator(1,type="o",col="magenta")
  pend <- locator(1,type="o",col="cyan")
  arrows(pstart$x,pstart$y,pend$x,pend$y,col="white")
  
  cat(pend$y-pstart$y,"\t")
  cat(pend$x-pstart$x,"\t")
  cat((pend$y-pstart$y)/(pend$x-pstart$x),"\n")
  
  
  # estAngle is the clockwise angle to rotate afterwards
  estAngle <- atan((pend$y-pstart$y)/(pend$x-pstart$x))*180/pi
  #   estAngle <- atan2((pend$y-pstart$y),(pend$x-pstart$x)) #*180/pi
  #   cat(estAngle)
  
  quad1 <- (pend$y>=pstart$y) && (pend$x>=pstart$x)
  quad2 <- (pend$y>=pstart$y) && (pend$x<pstart$x)
  quad3 <- (pend$y<pstart$y) && (pend$x<pstart$x)
  quad4 <- (pend$y<pstart$y) && (pend$x>=pstart$x)
  cat(estAngle)
  
  if(quad1 | quad4) estAngle <- estAngle
  if(quad2 | quad3) estAngle <- estAngle + 180 # + pi/2 # +180
  
  return(estAngle)
}

# estA <- estimateAngle() # useless YET! - the EBImage is not able to deal with pixel coordinates interactively AND correctly..
