showMe <- function(imgObject, dispMet="raster",...)
{
  display(imgObject, method=dispMet)
}
################
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




################
createBGimages <- function(allImagesToAverage,foundImgs,processingFolder,imgNames)
{
  # the images are in foundImgs
  
  firstImage <- readImage(foundImgs[1])
  summedUpImg <- firstImage > 5 # set it all black
  
  # create the average img and write it to file
  for (i in 1:length(foundImgs))
  {
    currentImg <- readImage(foundImgs[i])
    
    summedUpImg <- summedUpImg + currentImg
    cat("Done summing up image",imgNames[i],"\n")
  }
  
  avgImage <- summedUpImg / length(foundImgs)
  writeImage(avgImage[,,1],file=paste(processingFolder,"/backgroundImages/background_red_time_averaged.tif",sep=""))
  writeImage(avgImage[,,2],file=paste(processingFolder,"/backgroundImages/background_green_time_averaged.tif",sep=""))
  writeImage(avgImage,file=paste(processingFolder,"/backgroundImages/background_allchannels_time_averaged.tif",sep=""))
  
  # create by subtraction the bg-removed images and write them out
  for (i in 1:length(foundImgs))
  {
    currentImg <- readImage(foundImgs[i])
    bgRemovedImg <- currentImg - avgImage
    writeImage(bgRemovedImg[,,1],file=paste(processingFolder,"/backgroundsubtractedImages_red/bgRem_red_",imgNames[i],sep=""))
    writeImage(bgRemovedImg[,,2],file=paste(processingFolder,"/backgroundsubtractedImages_green/bgRem_green_",imgNames[i],sep=""))
    writeImage(bgRemovedImg,file=paste(processingFolder,"/backgroundsubtractedImages_allchannels/bgRem_",imgNames[i],sep=""))
    cat("Done with writing bg_subtracted image",imgNames[i],"\n")
  }
  
  
}




################

cutOutImages <- function(inputImgFiles,processingFolder=processingFolder,imgNames,
                         cutLeft=5,cutRight=5,cutUp=5,cutDown=5,write=F,cutAll=0)
{
  # sanity checks that we are not cutting  too much?
  ## if... stop, and print a message
  
  if(cutAll > 0)
  {
    cutLeft <- cutRight <- cutUp <- cutDown <- cutAll
  }
  
  for(i in 1:length(inputImgFiles))
  {
    img <- readImage(inputImgFiles[i])
    cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
    if(write)
    {
      writeImage(cutoutImg,file=paste0(processingFolder,"/cutoutImages/cutout_",imgNames[i]))
      cat("Done with writing cutout image",imgNames[i],"of",length(inputImgFiles), "\n")
    } else {
      showMe(cutoutImg)
      browser()
    }
  }
  
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

################
rotateImages <- function(inputImgFiles,processingFolder,imgNames,rotAngle=estAngle,write=F,tryOne=T)
{
  # sanity checks that we are not cutting  too much?
  ## if... stop, and print a message
  endLoop <- length(inputImgFiles)
  if (tryOne) endLoop <- 1
  
  for(i in 1:endLoop)
  {
    img <- readImage(inputImgFiles[i])
    rotImg <- rotate(img,rotAngle,
                     output.origin=c(dim(img)[1]/3,dim(img)[2]/3),
                     output.dim=c(dim(img)[1]*1.5,dim(img)[2]*1.5)
    )
    if(write && !tryOne)
    {
      writeImage(rotImg,file=paste0(processingFolder,"/rotatedImages/rotated_",imgNames[i]))
      cat("Done with writing rotated image",imgNames[i],"of",length(inputImgFiles), "\n")
    } else {
      showMe(rotImg)
      #       browser()
    }
  }
  
}
# rotateImages(inputImgFiles=foundImgs,processingFolder=paste(projectFolder,analysisFolder4,sep=""),imgNames=imgsJustNames,rotAngle=estA,write=F,tryOne=T)
# 
# rotateImages(inputImgFiles=foundImgs,processingFolder=paste(projectFolder,analysisFolder4,sep=""),imgNames=imgsJustNames,rotAngle=130,write=F,tryOne=T)
# 
# showMe(rotate(testimg2,130))

################
fullPreprocessWithWatershed_v1 <- function(filename="",imgname="",dispMet="raster",offsetGreen=0.15,offsetRed=0.15,writereport=FALSE,displayprocessing=FALSE,
                                        areaThresholdMin=5, areaThresholdMax=100,foundImgs="") #,...)
{
  rawimg <- readImage(filename)
  
  if (displayprocessing)    showMe(rawimg)
  rawimg_red <- rawimg[,,1]
  rawimg_green <- rawimg[,,2]
#   if (displayprocessing)    showMe(EBImage::combine(rgbImage(red=rawimg_red),rgbImage(green=rawimg_green),along=2))
  
  flo = makeBrush(3, shape='disc', step=FALSE)^2
  flo <- flo/sum(flo)
  
  thre_red <- thresh(filter2(rawimg_red,flo),w=10,h=10,offset=0.15)
  thre_green <- thresh(filter2(rawimg_green,flo),w=10,h=10,offset=0.15)
  
  # if needed with a step of smoothing & co
  kern <- makeBrush(size=3,shape="disc")
  
  
  dmthre_green <- distmap(thre_green)
  wsthre_green <- watershed(dmthre_green)
  dmthre_red <- distmap(thre_red)
  wsthre_red <- watershed(dmthre_red)
  
  
  
  
  
  
  segm <- wsthre_green
  buildingUp_green <- rawimg
  for(obj in 1:max(segm))
  {
    
    elab_singleObject <- segm
    elab_singleObject[segm!=obj]<- 0  
    buildUp_green <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp_green,col=colors()[52+obj])
    buildingUp_green <- buildUp_green
  }
  
  segm <- wsthre_red
  buildingUp_red <- rawimg
  for(obj in 1:max(segm))
  {
    
    elab_singleObject <- segm
    elab_singleObject[segm!=obj]<- 0  
    buildUp_red <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp_red,col=colors()[52+obj])
    buildingUp_red <- buildUp_red
  }
  
  
  
  
  #   segm_red <- bwlabel(thre_red)
  #   segm_green <- bwlabel(thre_green)
  segm_red <- wsthre_red
  segm_green <- wsthre_green
  
  
  
  #
  if (displayprocessing)    showMe(rgbImage(red=segm_red,green=segm_green))
  
  # first round painting all objects
  
  
  #   obj_green <- paintObjects(Image(segm_green,colormode="Grayscale"),rawimg_green)
  obj_green <- buildingUp_green
  
  
  
  #   obj_red <- paintObjects(Image(segm_red,colormode="Grayscale"),rawimg_red)
  obj_red <- buildingUp_red
  
  #   showMe(rgbImage(green=obj_green,red=obj_red))
  
  # filtering on the red channel
  shapeAndMomfeats_red <- as.data.frame(computeFeatures(segm_red,rawimg_red,methods.ref=c("computeFeatures.basic"),xname="cell"))
  allAreas_red <- shapeAndMomfeats_red$cell.0.s.area
  indexes_red <- 1:nrow(shapeAndMomfeats_red)
  enoughAreas_red <- which((allAreas_red > areaThresholdMin) & (allAreas_red < areaThresholdMax)) # maxbe different between cells on different channels
  notEnoughAreas_red <- setdiff(indexes_red,enoughAreas_red)
  notForShape_red <- c() # we need to define what else will be kicked out
  filterOut_red <- union(notEnoughAreas_red,notForShape_red)
  
  # filtering on the green channel
  shapeAndMomfeats_green <- as.data.frame(computeFeatures(segm_green,rawimg_green,methods.ref=c("computeFeatures.basic"),xname="cell"))
  allAreas_green <- shapeAndMomfeats_green$cell.0.s.area
  indexes_green <- 1:nrow(shapeAndMomfeats_green)
  enoughAreas_green <- which((allAreas_green > areaThresholdMin) & (allAreas_green < areaThresholdMax))
  notEnoughAreas_green <- setdiff(indexes_green,enoughAreas_green)
  notForShape_green <- c() # we need to define what else will be kicked out
  filterOut_green <- union(notEnoughAreas_green,notForShape_green)
  
  # put to zero what is filtered out as not interesting
  elab_red <- segm_red
  for(i in filterOut_red)
  {
    elab_red[elab_red==i] <- 0
  }
  elab_green <- segm_green
  for(i in filterOut_green)
  {
    elab_green[elab_green==i] <- 0
  }
  
  cells_red <- paintObjects(Image(elab_red,colormode="Grayscale"),rawimg_red)
  cells_green <- paintObjects(Image(elab_green,colormode="Grayscale"),rawimg_green)
  
  
  
  # old one
  if (displayprocessing)    showMe(rgbImage(green=obj_green,red=obj_red))
  # with filtering
  if (displayprocessing)    showMe(rgbImage(green=cells_green,red=cells_red))
  # compare one above other
#   allImgs <- EBImage::combine(rawimg,rgbImage(green=obj_green,red=obj_red),rgbImage(green=cells_green,red=cells_red))
#   segmImgs <- EBImage::combine(rawimg,rgbImage(red=segm_red),rgbImage(green=segm_green))
#   if (displayprocessing)    display(allImgs,method="raster",all=TRUE)
  
  # only if TRUE, produce images/reports/feature analysis table & co.
  # using the pruned filename - removed the path
  
  #     if (exists("foundImgs") & exists("imgsJustNames") )
  #     imgName <- imgsJustNames[match(filename,foundImgs)]
  #     else 
  #     imgName <- imgname
  #     
  # very very RAW!!!
  shatteredPieces <- strsplit(filename,"/")[[1]]
  imgName <- shatteredPieces[length(shatteredPieces)]
  if(writereport > 0)
  {
    writeImage(rgbImage(red=segm_red),file=paste(processingFolder,"/segmentedImages_red/segmented_",imgName,sep=""))
    writeImage(rgbImage(green=segm_green),file=paste(processingFolder,"/segmentedImages_green/segmented_",imgName,sep=""))
    
    writeImage(obj_red,file=paste(processingFolder,"/paintedObjects_red/objRed_",imgName,sep=""))
    writeImage(obj_green,file=paste(processingFolder,"/paintedObjects_green/objGreen_",imgName,sep=""))
    writeImage(cells_red,file=paste(processingFolder,"/paintedCells_red/cellRed_",imgName,sep=""))
    writeImage(cells_green,file=paste(processingFolder,"/paintedCells_green/cellGreen_",imgName,sep=""))
    
    
#     if (displayprocessing)  display(segmImgs,method="raster",all=TRUE)
#     pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
#     display(segmImgs,method="raster",all=TRUE)
#     dev.off()
#     if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
#     pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
#     display(allImgs,method="raster",all=TRUE)
#     dev.off()
    write.table(shapeAndMomfeats_red,file=paste(processingFolder,"/featuresReports_red/feats_mom_",imgName,".tsv",sep="")
                ,sep="\t",quote=FALSE,row.names=T,col.names=T)
    write.table(shapeAndMomfeats_green,file=paste(processingFolder,"/featuresReports_green/feats_mom_",imgName,".tsv",sep="")
                ,sep="\t",quote=FALSE,row.names=T,col.names=T)
  } else
  {
    if (displayprocessing)  browser()
  }
  
  
  
#   if (exists("foundImgs") & exists("imgsJustNames") )
    cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
}


################

preprocessTransmigrating <- function(filename="",imgname="",dispMet="raster",offset=0.15,writereport=FALSE,
                                     displayprocessing=FALSE,segmMethod="adaptive",useOpening=FALSE) #,...)
{
  rawimg <- readImage(filename,type="jpeg")
  
  if (displayprocessing)    showMe(rawimg)
  
  flo = makeBrush(3, shape='disc', step=FALSE)^2
  flo <- flo/sum(flo)
  
  adasmooththre <- thresh(filter2(rawimg,flo),w=15,h=15,offset=0.15)
  adathre <- thresh(rawimg,w=15,h=15,offset=0.1)
  otsuthre <- rawimg > oootsu(rawimg)
  kittlerthre <- rawimg > kittykitty(rawimg)
  
  threshComparison <- EBImage::combine(rawimg,adathre,adasmooththre,otsuthre,kittlerthre,along=2)
  # if needed with a step of opening/closing & co
  kern <- makeBrush(size=3,shape="disc")
  
  if(segmMethod=="adaptiveSmoothed")
    tosegm <- adasmooththre 
  
  if(segmMethod=="adaptive")
    tosegm <- adathre # possible to choose others
  
  if(segmMethod=="otsu")
    tosegm <- otsuthre # possible to choose others
  
  if(segmMethod=="kittler")
    tosegm <- kittlerthre # possible to choose others
  #   
  if(useOpening)
  {
    segm <- bwlabel(fillHull(opening(tosegm,kern)))
  } else
    segm <- bwlabel(fillHull(tosegm))
  
  
  if (displayprocessing)    showMe(segm/max(segm))
  
  # first round painting all objects
  
  obj <- paintObjects(segm,rgbImage(green=rawimg),col='#ff00ff')
  if (displayprocessing)    showMe(obj)
  
  # filtering on the red channel
  shapeAndMomfeats <- as.data.frame(computeFeatures(segm,rawimg,methods.ref="computeFeatures.basic",xname="cell"))
  allAreas <- shapeAndMomfeats$cell.0.s.area
  indexes <- 1:nrow(shapeAndMomfeats)
  enoughAreas <- which((allAreas > 2) & (allAreas < 1000))
  notEnoughAreas <- setdiff(indexes,enoughAreas)
  notForShape <- c() # we need to define what else will be kicked out
  filterOut <- union(notEnoughAreas,notForShape)
  
  # put to zero what is filtered out as not interesting
  elab <- segm
  for(i in filterOut)
  {
    elab[elab==i] <- 0
  }
  
  cells <- paintObjects(elab,rgbImage(green=rawimg),col='#ff00ff')
  cfr <- EBImage::combine(obj,cells,along=2)
  # with filtering
  if (displayprocessing)    showMe(cfr)
  
  
  
  # compare one above other
  allImgs <- EBImage::combine(rgbImage(green=rawimg),obj,cells,along=2)
  if (displayprocessing)    display(allImgs,method="raster")
  
  
  ## from here on
  # only if TRUE, produce images/reports/feature analysis table & co.
  # using the pruned filename - removed the path
  if (exists("foundImgs") & exists("imgsJustNames") )
    imgName <- imgsJustNames[match(filename,foundImgs)]
  else 
    imgName <- imgname
  
  if(writereport > 0)
  {
    writeImage(rgbImage(green=segm),file=paste(processingFolder,"/segmentedImages/segmented_",imgName,sep=""))
    
    writeImage(obj,file=paste(processingFolder,"/paintedObjects/obj_",imgName,sep=""))
    writeImage(cells,file=paste(processingFolder,"/paintedCells/cells_",imgName,sep=""))
    
    
    if (displayprocessing)  display(threshComparison,method="raster",all=TRUE)
    pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
    display(threshComparison,method="raster",all=TRUE)
    dev.off()
    if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
    pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
    display(allImgs,method="raster",all=TRUE)
    dev.off()
    write.table(shapeAndMomfeats,file=paste(processingFolder,"/featuresReports/feats_mom_",imgName,".tsv",sep="")
                ,sep="\t",quote=FALSE,row.names=T,col.names=T)
    
  } else
  {
    if (displayprocessing)  browser()
  }
  
  
  
  if (exists("foundImgs") & exists("imgsJustNames") )
    cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
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

processFeats <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
{
  framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
  areas <- framestats$cell.0.s.area
  nobjects <- nrow(framestats)
  shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
  #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
  # better like this
  # list exclusion criteriums
  notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
  notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
  notBecauseOfShape <- c()
  # more to come with higher resolution?
  
  # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
  leftOut <- union(notBecauseOfArea,notBecauseOfEccen) #, not because of ....
  candidates <- framestats[-leftOut,]
  cat("Found",nobjects,"objects to begin with ------\n")
  cat("Eliminated",length(notBecauseOfArea),"because of the area;",length(notBecauseOfEccen),"because of the eccentricity;",length(leftOut),"in total\n")
  cat("Frame",j,"\t\t",nobjects-length(leftOut),"potential cells according to area were identified!\n\n")
  # other stuff, e.g. save output to some log? # e.g. whatever is being cat'd, append it to a log file
  # e.g.
  # 2x2, images painted
  # below, the histograms for the areas
  #   
  #   par(mfrow=c(2,2))
  #   pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
  #   hist(areas) 
  #   hist(areas[areas>areaThreshold]) # & other criteria
  
}


reallyProcessReport <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
{
  framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
  areas <- framestats$cell.0.s.area
  nobjects <- nrow(framestats)
  shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
  #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
  # better like this
  # list exclusion criteriums
  notBecauseOfArea <- c()
  notBecauseOfEccen <- c()
  #   notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
  #   notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
  #   
  notBecauseOfShape <- c()
  # more to come with higher resolution?
  
  # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
  leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
  if(!is.null(leftOut))
  {
    candidates <- framestats[-leftOut,]
  } else
    candidates <- framestats
  
  cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
  write.table(candidates,
              file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
              row.names=FALSE, sep="\t"
  )
}




processFeats2atOnce <- function(idx,reportFilesFolder,objImageFolder,cellImageFolder,
                                areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,
                                folderPostprocessing="")
{
  featsRepList <- list.files(path=reportFilesFolder,full.names=T)
  objImgList <- list.files(path=objImageFolder,full.names=T)
  cellImgList <- list.files(path=cellImageFolder,full.names=T)
  
  # will use idx to navigate through them
  
  
  framestats <- read.delim(featsRepList[idx],sep="\t",stringsAsFactors=FALSE,header=TRUE)
  
  areas <- framestats$cell.0.s.area
  nobjects <- nrow(framestats)
  shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
  #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
  # better like this
  # list exclusion criteriums
  notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
  notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
  notBecauseOfShape <- c()
  # more to come with higher resolution?
  
  # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
  leftOut <- union(notBecauseOfArea,notBecauseOfEccen) #, not because of ....
  candidates <- framestats[-leftOut,]
  cat("Eliminated",length(notBecauseOfArea),"because of the area;",length(notBecauseOfEccen),"because of the eccentricity;",length(leftOut),"in total\n")
  cat("Frame",idx,"\t\t",nobjects-length(leftOut),"potential cells according to area were identified!\n\n")
  # other stuff, e.g. save output to some log? # e.g. whatever is being cat'd, append it to a log file
  # e.g.
  # 2x2, images painted
  # below, the histograms for the areas
  
  
  pdf(file=paste(folderPostprocessing,"/image",idx,".pdf",sep="")) # to stick to the names mentioned # eventually take the substring?
  par(mfrow=c(2,2))
  hist(areas) 
  hist(areas[areas>areaThreshold]) # & other criteria
  display(readImage(objImgList[idx]),method="raster")
  display(readImage(cellImgList[idx]),method="raster")
  dev.off()
  # somehow it is not displayes in the same page, dammit! -> use one figure? with EBI::combine
}





# for(j in feats_list_green)
# {
#   processFeats(j)
# }
# 
# 
# featsRepList <- list.files(path="./featuresReports_green/",full.names=T)
# objImgList <- list.files(path="./featuresReports_green/",full.names=T)
# celImgList <- list.files(path="./featuresReports_green/",full.names=T)
# 
# for(k in 1:100)
# {
#   processFeats2atOnce(idx=k,
#                       reportFilesFolder="featuresReports_green/",
#                       objImageFolder="paintedObjects_green/",
#                       cellImageFolder="paintedCells_green/",
#                       folderPostprocessing="postprocessing_green/")
# }


################
################ TRACKING ATTEMPTS #####################
# start with
# feats_list_green # for example

processReport <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
{
  framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
  areas <- framestats$cell.0.s.area
  nobjects <- nrow(framestats)
  shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
  #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
  # better like this
  # list exclusion criteriums
  notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
  notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
  
  notBecauseOfShape <- c()
  # more to come with higher resolution?
  
  # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
  leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
  if(!is.null(leftOut))
  {
    candidates <- framestats[-leftOut,]
  } else
    candidates <- framestats
  
  cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
  write.table(candidates,
              file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
              row.names=FALSE, sep="\t"
  )
}

# for(j in feats_list_green)
# {
#   processReport(j,folderPostprocessing="processedReports_green/",areaThreshold=3,shapeThreshold=0.5,eccenThreshold=1)
# }





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



################

oootsu <- function (input_image,nr_bit=16)  # use all channels?
{
  # image is read through readImage from EBImage
  # returns the level where the variance between classes is maximized
  ##STEPS NOT NEEDED!!!!    
  # transform into grey levels
  #  greyImg <- EBImage::channel(input_image,"grey")
  # the matrix gets stretched into a vec
  #  greyVec <- as.vector(greyImg)
  
  
  ## in order to treat properly the different channels, "step out like this"
  
  greyVec <- input_image
  totPixels <- length(greyVec)
  # and the values are rescaled
  #  greyLevels <- greyVec * 255
  #  greyLevels <- round(greyLevels)
  
  #  tableGrey <- tabulate(greyLevels)
  # tableGrey <- tableGrey + 0.0001
  
  #  n_i <- tableGrey
  #  p_i <- n_i/totPixels
  # careful, sum is not exactly 1...
  
  ## coding in 16 bit, slightly better
  greyLevels <- greyVec * (2^nr_bit -1) # parametrizable? nrBit <- 16
  
  #### modified!!
  greyLevels <- as.vector(greyLevels)
  
  
  greyLevels <- round(greyLevels)
  
  tableGrey <- tabulate(greyLevels)
  tableGrey <- tableGrey + 0.000001
  
  n_i <- tableGrey
  p_i <- n_i/totPixels
  # careful, sum is not exactly 1...
  
  ## defining functions for means and probabilities - what goes afterwards in the otsu formula to maximize
  ##
  mean_T <- function(p, L) #L=length(tableGrey) )
  {
    ind <- 1:L
    s <- ind * p[1:L]
    z <- sum(s)
    return(z)
  }
  ##
  mean_k <- function(p, k, L)
  {
    ind <- 1:k
    s <- ind * p[1:k]
    z <- sum(s)
    return(z)
  }
  ##
  w_k <- function(p, k)
  {
    s <- sum(p[1:k])
    return(s)
  }
  
  ## define the function to optimize
  sigma_B_k <- function(p, L, k)
  {
    ( mean_T(p,L) * w_k(p,k) - mean_k(p,k) )^2 / ( w_k(p,k) * (1 - w_k(p,k)) )
  }
  
  ## actually do the optimization (max required)
  o <- optimize(sigma_B_k, c(1,length(tableGrey)), maximum=TRUE, L=length(tableGrey), p=p_i )
  otsu_threshold <- o$maximum/(2^nr_bit-1) 
  
  cat("Calculated value with Otsu thresholding method...")
  cat(otsu_threshold)
  cat("\n")
  
  return(otsu_threshold)
  
}


################
kittykitty <- function (input_image,nr_bit=8)  # use all channels?
{
  greyVec <- input_image
  totPixels <- length(greyVec)
  ## coding in 16 bit, slightly better
  greyLevels <- greyVec * (2^nr_bit -1) # parametrizable? nrBit <- 16
  # greyLevels <- greyVec * (2^8 -1) # parametrizable? nrBit <- 16
  #### modified!!
  greyLevels <- as.vector(greyLevels)
  greyLevels <- round(greyLevels)
  
  tableGrey <- tabulate(greyLevels)
  tableGrey <- tableGrey + 0.000001
  
  
  n_i <- tableGrey
  p_i <- n_i/totPixels
  # careful, sum is not exactly 1...
  
  
  
  
  P_t <- function(p, k)
  {
    s <- sum(p[0:k])
    return(s)
  }
  
  
  ##
  mean_f <- function(p, k, L)
  {
    ind <- 1:k
    s <- ind * p[1:k]
    z <- sum(s)
    return(z)
  }
  
  mean_b <- function(p, k, L)
  {
    ind <- (k+1):L
    s <- ind * p[(k+1):L]
    z <- sum(s)
    return(z)
  }
  
  sigma_f_t <- function(p, k, L)
  {
    s <- 0
    mu <- mean_f(p,k,L)
    for (i in 1:k)
    {
      s <- s + ( (i - mu)^2 * p[i] )
    }
    return(sqrt(s))
  }
  
  sigma_b_t <- function(p, k, L)
  {
    s <- 0
    mu <- mean_f(p,k,L)
    for (i in (k+1):L)
    {
      s <- s + ( (i - mu)^2 * p[i] )
    }
    return(sqrt(s))
  }
  
  
  ## define the function to optimize
  kittIll <- function(p, L, k)
  {
    P_t(p,k)*log(sigma_f_t(p,k,L)) + (1-P_t(p,k))*log(sigma_b_t(p,k,L)) - P_t(p,k)*log(P_t(p,k)) - (1-P_t(p,k))*log((1-P_t(p,k)))
  }
  
  ## actually do the optimization (max required)
  o <- optimize(kittIll, c(1,length(tableGrey)), maximum=FALSE, L=length(tableGrey), p=p_i )
  kitty_threshold <- o$minimum/((2^nr_bit)-1) 
  
  cat("Calculated value with Kittler-Illinger thresholding method...")
  cat(kitty_threshold)
  cat("\n")
  
  return(kitty_threshold)
  
  
  
  
}




################
listImages <- function(repositoryFolder,fullPattern=T,searchPattern="*tif")
{
  list.files(path=repositoryFolder, pattern=searchPattern,full.names=fullPattern)
}


################
processingOverview <- function(filename="",imgname="",dispMet="raster",offsetGreen=0.15,offsetRed=0.15,writereport=FALSE,
                               displayprocessing=FALSE,
                               areaThresholdMin=5, areaThresholdMax=100,foundImgs="",
                               channel="",
                               brushSize=3,
                               adaptiveWidth=10,adaptiveHeight=10,adaptiveOffset=0.15,
                               kernSize=3) #,...)
{
  rawimg <- readImage(filename)
  #   if (displayprocessing)    showMe(rawimg)
  rawimg_red <- rawimg[,,1]
  rawimg_green <- rawimg[,,2]
  
  #   if (displayprocessing)    showMe(EBImage::combine(rgbImage(red=rawimg_red),rgbImage(green=rawimg_green),along=2))
  flo = makeBrush(brushSize, shape='disc', step=FALSE)^2
  flo <- flo/sum(flo)
  
  thre_red <- thresh(filter2(rawimg_red,flo),w=adaptiveWidth,h=adaptiveHeight,offset=adaptiveOffset)
  thre_green <- thresh(filter2(rawimg_green,flo),w=adaptiveWidth,h=adaptiveHeight,offset=adaptiveOffset)
  
  # if needed with a step of smoothing & co
  kern <- makeBrush(size=kernSize,shape="disc")
  
  # calculating the distance maps to initialize the watershed & performing the watershed transformation
  dmthre_green <- distmap(thre_green)
  wsthre_green <- watershed(dmthre_green)
  dmthre_red <- distmap(thre_red)
  wsthre_red <- watershed(dmthre_red)
  
  #
  
  segm <- wsthre_green
  buildingUp_green <- rawimg
  for(obj in 1:max(segm))
  {
    
    elab_singleObject <- segm
    elab_singleObject[segm!=obj]<- 0  
    buildUp_green <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp_green,col=colors()[52+obj])
    buildingUp_green <- buildUp_green
  }
  
  segm <- wsthre_red
  buildingUp_red <- rawimg
  for(obj in 1:max(segm))
  {
    
    elab_singleObject <- segm
    elab_singleObject[segm!=obj]<- 0  
    buildUp_red <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp_red,col=colors()[52+obj])
    buildingUp_red <- buildUp_red
  }
  
  
  
  
  #   segm_red <- bwlabel(thre_red)
  #   segm_green <- bwlabel(thre_green)
  segm_red <- wsthre_red
  segm_green <- wsthre_green
  
  
  
  #
  if (displayprocessing)    showMe(rgbImage(red=segm_red,green=segm_green))
  
  # first round painting all objects
  
  
  #   obj_green <- paintObjects(Image(segm_green,colormode="Grayscale"),rawimg_green)
  obj_green <- buildingUp_green
  
  
  
  #   obj_red <- paintObjects(Image(segm_red,colormode="Grayscale"),rawimg_red)
  obj_red <- buildingUp_red
  
  #   showMe(rgbImage(green=obj_green,red=obj_red))
  
  # filtering on the red channel
  shapeAndMomfeats_red <- as.data.frame(computeFeatures(segm_red,rawimg_red,methods.ref=c(),xname="cell"))
  allAreas_red <- shapeAndMomfeats_red$cell.0.s.area
  indexes_red <- 1:nrow(shapeAndMomfeats_red)
  enoughAreas_red <- which((allAreas_red > areaThresholdMin) & (allAreas_red < areaThresholdMax)) # maxbe different between cells on different channels
  notEnoughAreas_red <- setdiff(indexes_red,enoughAreas_red)
  notForShape_red <- c() # we need to define what else will be kicked out
  filterOut_red <- union(notEnoughAreas_red,notForShape_red)
  
  # filtering on the green channel
  shapeAndMomfeats_green <- as.data.frame(computeFeatures(segm_green,rawimg_green,methods.ref=c(),xname="cell"))
  allAreas_green <- shapeAndMomfeats_green$cell.0.s.area
  indexes_green <- 1:nrow(shapeAndMomfeats_green)
  enoughAreas_green <- which((allAreas_green > areaThresholdMin) & (allAreas_green < areaThresholdMax))
  notEnoughAreas_green <- setdiff(indexes_green,enoughAreas_green)
  notForShape_green <- c() # we need to define what else will be kicked out
  filterOut_green <- union(notEnoughAreas_green,notForShape_green)
  
  # put to zero what is filtered out as not interesting
  elab_red <- segm_red
  for(i in filterOut_red)
  {
    elab_red[elab_red==i] <- 0
  }
  elab_green <- segm_green
  for(i in filterOut_green)
  {
    elab_green[elab_green==i] <- 0
  }
  
  cells_red <- paintObjects(Image(elab_red,colormode="Grayscale"),rawimg_red)
  cells_green <- paintObjects(Image(elab_green,colormode="Grayscale"),rawimg_green)
  
  
  
  # old one
  if (displayprocessing)    showMe(rgbImage(green=obj_green,red=obj_red))
  # with filtering
  if (displayprocessing)    showMe(rgbImage(green=cells_green,red=cells_red))
  # compare one above other
  allImgs <- EBImage::combine(rawimg,rgbImage(green=obj_green,red=obj_red),rgbImage(green=cells_green,red=cells_red))
  segmImgs <- EBImage::combine(rawimg,rgbImage(red=segm_red),rgbImage(green=segm_green))
  if (displayprocessing)    display(allImgs,method="raster",all=TRUE)
  
  # only if TRUE, produce images/reports/feature analysis table & co.
  # using the pruned filename - removed the path
  
  #     if (exists("foundImgs") & exists("imgsJustNames") )
  #     imgName <- imgsJustNames[match(filename,foundImgs)]
  #     else 
  #     imgName <- imgname
  #     
  # very very RAW!!!
  shatteredPieces <- strsplit(filename,"/")[[1]]
  imgName <- shatteredPieces[length(shatteredPieces)]
  if(writereport > 0)
  {
    writeImage(rgbImage(red=segm_red),file=paste(processingFolder,"/segmentedImages_red/segmented_",imgName,sep=""))
    writeImage(rgbImage(green=segm_green),file=paste(processingFolder,"/segmentedImages_green/segmented_",imgName,sep=""))
    
    writeImage(obj_red,file=paste(processingFolder,"/paintedObjects_red/objRed_",imgName,sep=""))
    writeImage(obj_green,file=paste(processingFolder,"/paintedObjects_green/objGreen_",imgName,sep=""))
    writeImage(cells_red,file=paste(processingFolder,"/paintedCells_red/cellRed_",imgName,sep=""))
    writeImage(cells_green,file=paste(processingFolder,"/paintedCells_green/cellGreen_",imgName,sep=""))
    
    
    if (displayprocessing)  display(segmImgs,method="raster",all=TRUE)
    pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
    display(segmImgs,method="raster",all=TRUE)
    dev.off()
    if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
    pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
    display(allImgs,method="raster",all=TRUE)
    dev.off()
    write.table(shapeAndMomfeats_red,file=paste(processingFolder,"/featuresReports_red/feats_mom_",imgName,".tsv",sep="")
                ,sep="\t",quote=FALSE,row.names=T,col.names=T)
    write.table(shapeAndMomfeats_green,file=paste(processingFolder,"/featuresReports_green/feats_mom_",imgName,".tsv",sep="")
                ,sep="\t",quote=FALSE,row.names=T,col.names=T)
  } else
  {
    if (displayprocessing)  browser()
  }
  
  
  
  if (exists("foundImgs") & exists("imgsJustNames") )
    cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
}



################

processReportForMOSAIC <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
{
  framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
  
  candidates <- framestats[,1:2]
  #   candidates$m0 <- rep(0,nrow(candidates))
  #     candidates$m2 <- rep(0,nrow(candidates))
  #   names(candidates) <- c("frame",j,"","")
  names(candidates) <- c("frame",j)
  
  
  cat(paste0("frame ",j-1,"\n"), file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""))
  write.table(round(candidates,3),
              file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
              row.names=FALSE, sep=" ",col.names=F,quote=F,append=T
  )
  
  
  
}


################
processReportForMOSAIC_2 <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
{
  framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
  areas <- framestats$cell.0.s.area
  nobjects <- nrow(framestats)
  shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
  #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
  # better like this
  # list exclusion criteriums
  notBecauseOfArea <- c()
  notBecauseOfEccen <- c()
  #   notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
  #   notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
  #   
  notBecauseOfShape <- c()
  # more to come with higher resolution?
  
  # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
  leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
  if(!is.null(leftOut))
  {
    candidates <- framestats[-leftOut,]
  } else
    candidates <- framestats
  
  cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
  write.table(candidates[,1:2],
              file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
              row.names=FALSE, sep=" ",col.names=F
  )
}

################
preprocessTransmigratingPlusWatershed <- function(filename="",imgname="",dispMet="raster",offset=0.15,writereport=FALSE,
                                                  displayprocessing=FALSE,segmMethod="adaptive",useOpening=FALSE) #,...)
{
  rawimg <- readImage(filename,type="jpeg")
  
  if (displayprocessing)    showMe(rawimg)
  
  flo = makeBrush(3, shape='disc', step=FALSE)^2
  flo <- flo/sum(flo)
  
  adasmooththre <- thresh(filter2(rawimg,flo),w=15,h=15,offset=0.15)
  adathre <- thresh(rawimg,w=15,h=15,offset=0.1)
  otsuthre <- rawimg > oootsu(rawimg)
  kittlerthre <- rawimg > kittykitty(rawimg)
  
  ################# RE_UNCOMMENT? ############ threshComparison <- EBImage::combine(rawimg,adathre,adasmooththre,otsuthre,kittlerthre,along=2)
  # if needed with a step of opening/closing & co
  kern <- makeBrush(size=3,shape="disc")
  
  if(segmMethod=="adaptiveSmoothed")
    tosegm <- adasmooththre 
  
  if(segmMethod=="adaptive")
    tosegm <- adathre # possible to choose others
  
  if(segmMethod=="otsu")
    tosegm <- otsuthre # possible to choose others
  
  if(segmMethod=="kittler")
    tosegm <- kittlerthre # possible to choose others
  #   
  if(useOpening)
  {
    segm <- bwlabel(fillHull(opening(tosegm,kern)))
  } else
    segm <- bwlabel(fillHull(tosegm))
  
  
  
  dmthre <- distmap(tosegm)
  wsthre <- watershed(dmthre)
  
  
  segm <- wsthre
  buildingUp <- rawimg
  for(obj in 1:max(segm))
  {
    
    elab_singleObject <- segm
    elab_singleObject[segm!=obj]<- 0  
    buildUp <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp,col=colors()[52+obj])
    buildingUp <- buildUp
  }
  
  watershedObjs <- buildingUp
  
  
  
  
  if (displayprocessing)    showMe(segm/max(segm))
  
  # first round painting all objects
  
  obj <- paintObjects(segm,rgbImage(green=rawimg),col='#ff00ff')
  if (displayprocessing)    showMe(obj)
  
  # filtering on the red channel
  shapeAndMomfeats <- as.data.frame(computeFeatures(segm,rawimg,methods.ref="computeFeatures.basic",xname="cell"))
  allAreas <- shapeAndMomfeats$cell.0.s.area
  indexes <- 1:nrow(shapeAndMomfeats)
  enoughAreas <- which((allAreas > 2) & (allAreas < 1000))
  notEnoughAreas <- setdiff(indexes,enoughAreas)
  notForShape <- c() # we need to define what else will be kicked out
  filterOut <- union(notEnoughAreas,notForShape)
  
  # put to zero what is filtered out as not interesting
  elab <- segm
  for(i in filterOut)
  {
    elab[elab==i] <- 0
  }
  
  cells <- paintObjects(elab,rgbImage(green=rawimg),col='#ff00ff')
  ################# RE_UNCOMMENT? ############ cfr <- EBImage::combine(obj,cells,along=2)
  # with filtering
  ################# RE_UNCOMMENT? ############ if (displayprocessing)    showMe(cfr)
  
  ######## NEW
  obj <- watershedObjs
  cells <- watershedObjs
  
  
  # compare one above other
  ################# RE_UNCOMMENT? ############   allImgs <- EBImage::combine(rgbImage(green=rawimg),obj,cells,along=2)
  ################# RE_UNCOMMENT? ############   if (displayprocessing)    display(allImgs,method="raster")
  
  
  ## from here on
  # only if TRUE, produce images/reports/feature analysis table & co.
  
  # very very RAW!!!
  shatteredPieces <- strsplit(filename,"/")[[1]]
  imgName <- shatteredPieces[length(shatteredPieces)]
  
  if(writereport > 0)
  {
    writeImage(rgbImage(green=segm),file=paste(processingFolder,"/segmentedImages/segmented_",imgName,sep=""))
    
    writeImage(obj,file=paste(processingFolder,"/paintedObjects/obj_",imgName,sep=""))
    writeImage(cells,file=paste(processingFolder,"/paintedCells/cells_",imgName,sep=""))
    
    
    ################# RE_UNCOMMENT? ############if (displayprocessing)  display(threshComparison,method="raster",all=TRUE)
    ################# RE_UNCOMMENT? ############ pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
    ################# RE_UNCOMMENT? ############ display(threshComparison,method="raster",all=TRUE)
    ################# RE_UNCOMMENT? ############ dev.off()
    ################# RE_UNCOMMENT? ############if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
    ################# RE_UNCOMMENT? ############ pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
    ################# RE_UNCOMMENT? ############ display(allImgs,method="raster",all=TRUE)
    ################# RE_UNCOMMENT? ############  dev.off()
    write.table(shapeAndMomfeats,file=paste(processingFolder,"/featuresReports/feats_mom_",imgName,".tsv",sep="")
                ,sep="\t",quote=FALSE,row.names=T,col.names=T)
    
  } else
  {
    if (displayprocessing)  browser()
  }
  
  
  
  #   if (exists("foundImgs") & exists("imgsJustNames") )
  cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
}


################
################
################
################
################
################
################
################
################
################
################
################
################
################
################

