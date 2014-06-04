
# to be used in combination with a binary image - segmented e.g. via watershed/other methods
reproducibleColorLabels <- function (x, normalize = TRUE) 
{
  set.seed(123)
  M <- max(x)
  R <- sample(M)
  G <- sample(M)
  B <- sample(M)
  ch1 = x
  ch2 = x
  ch3 = x
  ch1[ch1 > 0] <- R[ch1[ch1 > 0]]
  ch2[ch2 > 0] <- G[ch2[ch2 > 0]]
  ch3[ch3 > 0] <- B[ch3[ch3 > 0]]
  Img = Image(data = combine(ch1, ch2, ch3), colormode = "Color")
  if (normalize) {
    Img <- normalize(Img)
  }
  Img
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
