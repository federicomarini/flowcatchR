
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


