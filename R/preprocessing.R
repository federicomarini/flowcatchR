#' cut.FrameList
#' 
#' Performs cropping on the FrameList object, selecting how many pixels should be cut on each side
#' 
#' Cropping can be performed with careful choice of all cutting sides, or cropping a single value from
#' all sides
#' 
#' @param x An input FrameList object
#' @param cutLeft Amount of pixels to be cut at the side
#' @param cutRight Amount of pixels to be cut at the side
#' @param cutUp Amount of pixels to be cut at the side
#' @param cutDown Amount of pixels to be cut at the side
#' @param cutAll Amount of pixels to be cut at all sides. Overrides the single side values
#' @param testing Logical, whether to just test the cropping or to actually perform it. Default set to FALSE
#' @param ... Arguments to be passed to methods
#' 
#' @return A FrameList object, with cropped frames in the image slot
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014

cut.FrameList <- function(x,
                          cutLeft=5,cutRight=5,cutUp=5,cutDown=5,
                          cutAll=0,
                          testing=FALSE,
                          ...)
{
  out <- vector(length(x),mode="list")
  class(out) <- c("FrameList",class(out))
  if(cutAll > 0)
  {
    cutLeft <- cutRight <- cutUp <- cutDown <- cutAll
  }
  
  if(!testing)
  {
    for(i in 1:length(x))
    {
      img <- x[[i]]$image
      cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
      out[[i]]$image <- cutoutImg
      out[[i]]$location <- NA # it is modified from an existing object -> maybe provide the name of the object it got created from?
    }
    return(out)
  } else {
    # just check on one image, the first one
    img <- framelist[[1]]$image
    cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
    display(cutoutImg)
    return(cutoutImg)
  }
}


#' rotate.FrameList
#' 
#' Rotates all images in a FrameList object 
#' 
#' Rotation is performed exploiting the rotate function of the EBImage package. Could be automated if support for coordinate/pixel interaction is included
#' 
#' @param framelist A FrameList object
#' @param rotAngle The rotation angle (clockwise) specified in degrees
#' @param testing Logical, whether to just test the rotation or to actually perform it. Default set to FALSE
#' @param output.origin A vector of 2 numbers indicating the dimension of the output image, as in the rotate function
#' @param output.dim A vector of 2 numbers indicating the output coordinates of the origin in pixels, as in the rotate function
#'  
#' @return A FrameList object containing the rotated frames
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
rotate.FrameList <- function(framelist,
                             rotAngle=0,
                             testing=FALSE,
                             output.origin=c(dim(framelist[[i]]$image)[1]/3,dim(framelist[[i]]$image)[2]/3),
                             output.dim=c(dim(framelist[[i]]$image)[1]*1.5,dim(framelist[[i]]$image)[2]*1.5))
{
  out <- vector(length(framelist),mode="list")
  class(out) <- c("FrameList",class(out))
    
  if(!testing)
  {
    for(i in 1:length(framelist))
    {
      img <- framelist[[i]]$image
      rotatedImg <- rotate(img,rotAngle,
                           output.origin=output.origin,
                           output.dim=output.dim)
      out[[i]]$image <- rotatedImg
      out[[i]]$location <- NA
      
    } 
    return(out)
  } else {
    # just check on one image, the first one
    img <- framelist[[1]]$image
    rotatedImg <- rotate(img,rotAngle,
                         output.origin=output.origin,
                         output.dim=output.dim)
                  
    return(rotatedImg)
  }  
}










#' preprocess
#' 
#' Generic preprocessing function
#' 
#' Can be applied to FrameList or ChannelsFrameList objects. ChannelsFrameList are then subset to the chosen channel,
#' and the method preprocess.FrameList is then applied, with its set of parameters
#'  
#' @param x A FrameList or a ChannelsFrameList object
#' @param ... Arguments to be passed to methods, such as channel and/or preprocessing parameters
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
preprocess <- function(x,...)
{
  UseMethod("preprocess")
}


#' preprocess.ChannelsFrameList
#' 
#' Preprocessing function for ChannelsFrameList objects
#' 
#' ChannelsFrameList are then subset to the chosen channel, and the method preprocess.FrameList is then applied, with its set of parameters
#'  
#' @param channelsframelist A ChannelsFrameList object
#' @param channel Character string. The channel to perform the operations on. Can be "red", "green" or "blue"
#' 
#' @return A FrameList object, whose frame images are the preprocessed versions of the input images
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
preprocess.ChannelsFrameList <- function(channelsframelist,
                                         channel="") # HARALD: should i put here also the ...? and also in the function code too?
{
  cat("do that")
  # call on red OR
  # call on green OR
  # call on blue
  switch(channel,
         red={
           cat("Preprocessing the red channel!\n")
           out <- preprocess.FrameList(channelsframelist[[1]])
           }, 
         green={
           cat("Preprocessing the green channel!\n")
           out <- preprocess.FrameList(channelsframelist[[2]])
         },
         blue={
           cat("Preprocessing the blue channel!\n")
           out <- preprocess.FrameList(channelsframelist[[3]])
         },
         stop("You did not choose any of the value for the channel - allowed values= red | green | blue")
  )
  return(out)
}

#' preprocess.FrameList
#' 
#' Preprocessing function for FrameList objects
#' 
#' FrameList objects are processed according to the chosen set of parameters. Many of them refer directly to 
#' existing EBImage functions, please see the corresponding help for additional information
#'  
#' @param framelist A FrameList object
#' @param brushSize Size in pixels of the brush to be used for initial smoothing
#' @param brushShape Shape of the brush to be used for initial smoothing
#' @param adaptOffset Offset to be used in the adaptive thresholding step
#' @param adaptWinWidth Width of the window for the adaptive thresholding step
#' @param adaptWinHeight Height of the window for the adaptive thresholding step
#' @param kernSize Size in pixels of the kernel used for morphological operations
#' @param kernShape Shape of the kernel used for morphological operations
#' @param watershedTolerance Tolerance allowed in performing the watershed-based segmentation
#' @param watershedRadius Radius for the watershed-based segmentation
#' @param displayprocessing Logical, whether to display intermediate steps while performing preprocessing. Dismissed currently, it could increase runtime a lot
#' @param areaThresholdMin Size in pixels of the minimum area needed to detect the object as a potential particle of interest
#' @param areaThresholdMax Size in pixels of the maximum area allowed to detect the object as a potential particle of interest
#' 
#' @return A FrameList object, whose frame images are the preprocessed versions of the input images
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
preprocess.FrameList <- function(framelist,
                                 brushSize=3,
                                 brushShape="disc",
                                 adaptOffset=0.15,
                                 adaptWinWidth=10,
                                 adaptWinHeight=10,
                                 kernSize=3,
                                 kernShape="disc",
                                 watershedTolerance=1,
                                 watershedRadius=1,
                                 displayprocessing=FALSE,
                                 areaThresholdMin=5,
                                 areaThresholdMax=100) # for the single channel images/for one channel of multi-channel images
{
  cat("do this - processing the single channel")
  out <- vector(length(framelist),mode="list")
  class(out) <- c("FrameList",class(out))
  
  for(i in 1:length(framelist))
  {
    rawimg <- framelist[[i]]$image
    colorMode(rawimg) <- Grayscale
    
    flo = makeBrush(brushSize, brushShape, step=FALSE)^2
    flo <- flo/sum(flo)
    
    thresh_img <- thresh(filter2(rawimg,flo),w=adaptWinWidth,h=adaptWinHeight,offset=adaptOffset)
    
    # if needed with a step of smoothing & co (operations of opening,...)
    kern <- makeBrush(size=kernSize,shape=kernShape)
    
    distmap_thre <- distmap(thresh_img)
    watershed_thre <- watershed(distmap_thre,tolerance=watershedTolerance,ext=watershedRadius) 
  
    out[[i]]$image <- watershed_thre
    out[[i]]$channel <- framelist[[i]]$channel
    out[[i]]$location <- framelist[[i]]$location  # or maybe call it location raw? 
  }
  return(out)
}





#' extractParticles
#' 
#' Extract particles from the images of a FrameList object. 
#' 
#'  
#' @param framelistRaw A FrameList object with the raw images (mandatory)
#' @param framelistPreprocessed A FrameList object with preprocessed images (optional, if not provided gets produced with standard default parameters)
#' @param channel Character string. The channel to perform the operations on. Can be "red", "green" or "blue"
#' @param areaThresholdMin Size in pixels of the minimum area needed to detect the object as a potential particle of interest
#' @param areaThresholdMax Size in pixels of the maximum area allowed to detect the object as a potential particle of interest
#' 
#' @return A ParticleList object, containing all detected particles for each frame
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
extractParticles <- function(framelistRaw,
                             framelistPreprocessed=NULL,
                             channel="",  # if we provide the channelsFrameList as input 
                             areaThresholdMin=5,
                             areaThresholdMax=100 # and others of interest, for example
                             )
{
  if(!is(framelistRaw,"FrameList") & !is(framelistRaw,"ChannelsFrameList"))
  {
    stop("You need to provide at least a FrameList/channelsFrameList object as input!")
  }
  
  
  if(is.null(framelistPreprocessed))
  {
    cat("You did not provide a preprocessed FrameList alongside with the raw set of frames!\n")
    cat("Don't worry, the raw FrameList object will be first preprocessed with a set of default parameter!\n")
    cat("You can always change them afterwards if they do not fit to your scenario!")
    if(is(framelistRaw,"ChannelsFrameList"))
    {
      framelistRaw <- framelistRaw[[channel]]
      framelistProcessed <- preprocess.FrameList(framelistRaw)
    } else {
      framelistPreprocessed <- preprocess.FrameList(framelistRaw)
    }
  }
  
  # check that both input framelists have same length!
  if(length(framelistRaw) != length(framelistProcessed) )
  {
    stop("FrameList objects have different lengths!")
  } else {
    cat("Computing features...\n")
  }
  
  # returns a particle list - not linked yet
  
  out <- vector(length(framelistRaw),mode="list")
  class(out) <- c("ParticleList",class(out))
  
  for(i in 1:length(framelistRaw))
  {
    segmImg <- framelistProcessed[[i]]$image
    rawImg <- framelistRaw[[i]]$image
    
    imgFeatures <- as.data.frame(computeFeatures(segmImg,rawImg,xname="cell"))
    
    out[[i]]$particles <- imgFeatures
    out[[i]]$imgSource <- framelistProcessed[[i]]$location
    out[[i]]$channel <- framelistProcessed[[i]]$channel
  }
  cat("Done!\n")
  return(out)
}



#' filterParticles
#' 
#' Performs filtering on a ParticleList object
#' 
#' According to parameters of interests, such as size, eccentricity/shape, filters out the particles that do not 
#' satisfy the indicated requirements
#' 
#' @param particlelist A ParticleList object. A LinkedParticleList object can also be provided as input, yet the returned object will be a ParticleList object that 
#' @param areaThresholdMin Size in pixels of the minimum area needed to detect the object as a potential particle of interest
#' @param areaThresholdMax Size in pixels of the maximum area allowed to detect the object as a potential particle of interest
#' 
#' 
#' @return A ParticleList object
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
filterParticles <- function(particlelist,
                            areaThresholdMin = 1,
                            areaThresholdMax = 1000 #, # and others of interest, for example
                            #shapeThreshold = 0.5,
                            #eccentricityThreshold = 0.7 # currently not so efficient with so small particles available in the images!!
                            )
{
  # returns a particle list - not linked yet
  out <- vector(length(particlelist),mode="list")
  if(is(particlelist,"linkedParticleList"))
  {
    class(out) <- c("ParticleList",class(out))
    cat("Warning, you are filtering particles that were previously linked by tracking them - reperform the linking afterwards!\n")
  } else {
    if(is(particlelist,"ParticleList")) 
    {
      class(out) <- c("ParticleList",class(out))
      cat("Filtering the particles...\n")
    } else {
      stop("You need to provide a ParticleList object as input for filterParticles!\n")
    }
  }
  
  
  for(i in 1:length(particlelist))
  {
    candidateParticles <- particlelist[[i]]$particles
    
    nobjects <- nrow(candidateParticles)
    candidateParticles$shapeFactor <- (candidateParticles$cell.0.s.perimeter)^2 / (4*pi*candidateParticles$cell.0.s.area)
    
    notBecauseOfArea <- c()
    notBecauseOfEccen <- c()
    notBecauseOfShape <- c()
    
    notBecauseOfArea <- which( (candidateParticles$cell.0.s.area < areaThresholdMin) | (candidateParticles$cell.0.s.area > areaThresholdMax) )
#     notBecauseOfEccen <- which(candidateParticles$cell.0.m.eccentricity > eccenThreshold)
    
    leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
    if(length(leftOut)!=0)#     if(!is.null(leftOut))
    {
      filteredParticles <- candidateParticles[-leftOut,]
    } else {
      filteredParticles <- candidateParticles
    }
      #       cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
    
    
    out[[i]]$particles <- filteredParticles
    out[[i]]$imgSource <- particlelist[[i]]$imgSource
    out[[i]]$channel <- particlelist[[i]]$channel
  }
  return(out)
}










# 
# 
# ################
# ## old ##
# ## old ##
# ## old ##
# 
# # removeBackground
# ################
# createBGimages <- function(allImagesToAverage,foundImgs,processingFolder,imgNames)
# {
#   # the images are in foundImgs
#   
#   firstImage <- readImage(foundImgs[1])
#   summedUpImg <- firstImage > 5 # set it all black
#   
#   # create the average img and write it to file
#   for (i in 1:length(foundImgs))
#   {
#     currentImg <- readImage(foundImgs[i])
#     
#     summedUpImg <- summedUpImg + currentImg
#     cat("Done summing up image",imgNames[i],"\n")
#   }
#   
#   avgImage <- summedUpImg / length(foundImgs)
#   writeImage(avgImage[,,1],file=paste(processingFolder,"/backgroundImages/background_red_time_averaged.tif",sep=""))
#   writeImage(avgImage[,,2],file=paste(processingFolder,"/backgroundImages/background_green_time_averaged.tif",sep=""))
#   writeImage(avgImage,file=paste(processingFolder,"/backgroundImages/background_allchannels_time_averaged.tif",sep=""))
#   
#   # create by subtraction the bg-removed images and write them out
#   for (i in 1:length(foundImgs))
#   {
#     currentImg <- readImage(foundImgs[i])
#     bgRemovedImg <- currentImg - avgImage
#     writeImage(bgRemovedImg[,,1],file=paste(processingFolder,"/backgroundsubtractedImages_red/bgRem_red_",imgNames[i],sep=""))
#     writeImage(bgRemovedImg[,,2],file=paste(processingFolder,"/backgroundsubtractedImages_green/bgRem_green_",imgNames[i],sep=""))
#     writeImage(bgRemovedImg,file=paste(processingFolder,"/backgroundsubtractedImages_allchannels/bgRem_",imgNames[i],sep=""))
#     cat("Done with writing bg_subtracted image",imgNames[i],"\n")
#   }
#   
#   
# }
# 
# ################
# 
# cutOutImages <- function(inputImgFiles,processingFolder=processingFolder,imgNames,
#                          cutLeft=5,cutRight=5,cutUp=5,cutDown=5,write=F,cutAll=0)
# {
#   # sanity checks that we are not cutting  too much?
#   ## if... stop, and print a message
#   
#   if(cutAll > 0)
#   {
#     cutLeft <- cutRight <- cutUp <- cutDown <- cutAll
#   }
#   
#   for(i in 1:length(inputImgFiles))
#   {
#     img <- readImage(inputImgFiles[i])
#     cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
#     if(write)
#     {
#       writeImage(cutoutImg,file=paste0(processingFolder,"/cutoutImages/cutout_",imgNames[i]))
#       cat("Done with writing cutout image",imgNames[i],"of",length(inputImgFiles), "\n")
#     } else {
#       showMe(cutoutImg)
#       browser()
#     }
#   }
#   
# }
# 
# ################
# rotateImages <- function(inputImgFiles,processingFolder,imgNames,rotAngle=estAngle,write=F,tryOne=T)
# {
#   # sanity checks that we are not cutting  too much?
#   ## if... stop, and print a message
#   endLoop <- length(inputImgFiles)
#   if (tryOne) endLoop <- 1
#   
#   for(i in 1:endLoop)
#   {
#     img <- readImage(inputImgFiles[i])
#     rotImg <- rotate(img,rotAngle,
#                      output.origin=c(dim(img)[1]/3,dim(img)[2]/3),
#                      output.dim=c(dim(img)[1]*1.5,dim(img)[2]*1.5)
#     )
#     if(write && !tryOne)
#     {
#       writeImage(rotImg,file=paste0(processingFolder,"/rotatedImages/rotated_",imgNames[i]))
#       cat("Done with writing rotated image",imgNames[i],"of",length(inputImgFiles), "\n")
#     } else {
#       showMe(rotImg)
#       #       browser()
#     }
#   }
#   
# }
# # rotateImages(inputImgFiles=foundImgs,processingFolder=paste(projectFolder,analysisFolder4,sep=""),imgNames=imgsJustNames,rotAngle=estA,write=F,tryOne=T)
# # 
# # rotateImages(inputImgFiles=foundImgs,processingFolder=paste(projectFolder,analysisFolder4,sep=""),imgNames=imgsJustNames,rotAngle=130,write=F,tryOne=T)
# # 
# # showMe(rotate(testimg2,130))
# 
# ## old ##
# ## old ##
# fullPreprocessWithWatershed_v1 <- function(filename="",imgname="",dispMet="raster",offsetGreen=0.15,offsetRed=0.15,writereport=FALSE,displayprocessing=FALSE,
#                                            areaThresholdMin=5, areaThresholdMax=100,foundImgs="") #,...)
# {
#   rawimg <- readImage(filename)
#   
#   if (displayprocessing)    showMe(rawimg)
#   rawimg_red <- rawimg[,,1]
#   rawimg_green <- rawimg[,,2]
#   #   if (displayprocessing)    showMe(EBImage::combine(rgbImage(red=rawimg_red),rgbImage(green=rawimg_green),along=2))
#   
#   flo = makeBrush(3, shape='disc', step=FALSE)^2
#   flo <- flo/sum(flo)
#   
#   thre_red <- thresh(filter2(rawimg_red,flo),w=10,h=10,offset=0.15)
#   thre_green <- thresh(filter2(rawimg_green,flo),w=10,h=10,offset=0.15)
#   
#   # if needed with a step of smoothing & co
#   kern <- makeBrush(size=3,shape="disc")
#   
#   
#   dmthre_green <- distmap(thre_green)
#   wsthre_green <- watershed(dmthre_green)
#   dmthre_red <- distmap(thre_red)
#   wsthre_red <- watershed(dmthre_red)
#   
#   
#   
#   
#   
#   
#   segm <- wsthre_green
#   buildingUp_green <- rawimg
#   for(obj in 1:max(segm))
#   {
#     
#     elab_singleObject <- segm
#     elab_singleObject[segm!=obj]<- 0  
#     buildUp_green <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp_green,col=colors()[52+obj])
#     buildingUp_green <- buildUp_green
#   }
#   
#   segm <- wsthre_red
#   buildingUp_red <- rawimg
#   for(obj in 1:max(segm))
#   {
#     
#     elab_singleObject <- segm
#     elab_singleObject[segm!=obj]<- 0  
#     buildUp_red <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp_red,col=colors()[52+obj])
#     buildingUp_red <- buildUp_red
#   }
#   
#   
#   
#   
#   #   segm_red <- bwlabel(thre_red)
#   #   segm_green <- bwlabel(thre_green)
#   segm_red <- wsthre_red
#   segm_green <- wsthre_green
#   
#   
#   
#   #
#   if (displayprocessing)    showMe(rgbImage(red=segm_red,green=segm_green))
#   
#   # first round painting all objects
#   
#   
#   #   obj_green <- paintObjects(Image(segm_green,colormode="Grayscale"),rawimg_green)
#   obj_green <- buildingUp_green
#   
#   
#   
#   #   obj_red <- paintObjects(Image(segm_red,colormode="Grayscale"),rawimg_red)
#   obj_red <- buildingUp_red
#   
#   #   showMe(rgbImage(green=obj_green,red=obj_red))
#   
#   # filtering on the red channel
#   shapeAndMomfeats_red <- as.data.frame(computeFeatures(segm_red,rawimg_red,methods.ref=c("computeFeatures.basic"),xname="cell"))
#   allAreas_red <- shapeAndMomfeats_red$cell.0.s.area
#   indexes_red <- 1:nrow(shapeAndMomfeats_red)
#   enoughAreas_red <- which((allAreas_red > areaThresholdMin) & (allAreas_red < areaThresholdMax)) # maxbe different between cells on different channels
#   notEnoughAreas_red <- setdiff(indexes_red,enoughAreas_red)
#   notForShape_red <- c() # we need to define what else will be kicked out
#   filterOut_red <- union(notEnoughAreas_red,notForShape_red)
#   
#   # filtering on the green channel
#   shapeAndMomfeats_green <- as.data.frame(computeFeatures(segm_green,rawimg_green,methods.ref=c("computeFeatures.basic"),xname="cell"))
#   allAreas_green <- shapeAndMomfeats_green$cell.0.s.area
#   indexes_green <- 1:nrow(shapeAndMomfeats_green)
#   enoughAreas_green <- which((allAreas_green > areaThresholdMin) & (allAreas_green < areaThresholdMax))
#   notEnoughAreas_green <- setdiff(indexes_green,enoughAreas_green)
#   notForShape_green <- c() # we need to define what else will be kicked out
#   filterOut_green <- union(notEnoughAreas_green,notForShape_green)
#   
#   # put to zero what is filtered out as not interesting
#   elab_red <- segm_red
#   for(i in filterOut_red)
#   {
#     elab_red[elab_red==i] <- 0
#   }
#   elab_green <- segm_green
#   for(i in filterOut_green)
#   {
#     elab_green[elab_green==i] <- 0
#   }
#   
#   cells_red <- paintObjects(Image(elab_red,colormode="Grayscale"),rawimg_red)
#   cells_green <- paintObjects(Image(elab_green,colormode="Grayscale"),rawimg_green)
#   
#   
#   
#   # old one
#   if (displayprocessing)    showMe(rgbImage(green=obj_green,red=obj_red))
#   # with filtering
#   if (displayprocessing)    showMe(rgbImage(green=cells_green,red=cells_red))
#   # compare one above other
#   #   allImgs <- EBImage::combine(rawimg,rgbImage(green=obj_green,red=obj_red),rgbImage(green=cells_green,red=cells_red))
#   #   segmImgs <- EBImage::combine(rawimg,rgbImage(red=segm_red),rgbImage(green=segm_green))
#   #   if (displayprocessing)    display(allImgs,method="raster",all=TRUE)
#   
#   # only if TRUE, produce images/reports/feature analysis table & co.
#   # using the pruned filename - removed the path
#   
#   #     if (exists("foundImgs") & exists("imgsJustNames") )
#   #     imgName <- imgsJustNames[match(filename,foundImgs)]
#   #     else 
#   #     imgName <- imgname
#   #     
#   # very very RAW!!!
#   shatteredPieces <- strsplit(filename,"/")[[1]]
#   imgName <- shatteredPieces[length(shatteredPieces)]
#   if(writereport > 0)
#   {
#     writeImage(rgbImage(red=segm_red),file=paste(processingFolder,"/segmentedImages_red/segmented_",imgName,sep=""))
#     writeImage(rgbImage(green=segm_green),file=paste(processingFolder,"/segmentedImages_green/segmented_",imgName,sep=""))
#     
#     writeImage(obj_red,file=paste(processingFolder,"/paintedObjects_red/objRed_",imgName,sep=""))
#     writeImage(obj_green,file=paste(processingFolder,"/paintedObjects_green/objGreen_",imgName,sep=""))
#     writeImage(cells_red,file=paste(processingFolder,"/paintedCells_red/cellRed_",imgName,sep=""))
#     writeImage(cells_green,file=paste(processingFolder,"/paintedCells_green/cellGreen_",imgName,sep=""))
#     
#     
#     #     if (displayprocessing)  display(segmImgs,method="raster",all=TRUE)
#     #     pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
#     #     display(segmImgs,method="raster",all=TRUE)
#     #     dev.off()
#     #     if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
#     #     pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
#     #     display(allImgs,method="raster",all=TRUE)
#     #     dev.off()
#     write.table(shapeAndMomfeats_red,file=paste(processingFolder,"/featuresReports_red/feats_mom_",imgName,".tsv",sep="")
#                 ,sep="\t",quote=FALSE,row.names=T,col.names=T)
#     write.table(shapeAndMomfeats_green,file=paste(processingFolder,"/featuresReports_green/feats_mom_",imgName,".tsv",sep="")
#                 ,sep="\t",quote=FALSE,row.names=T,col.names=T)
#   } else
#   {
#     if (displayprocessing)  browser()
#   }
#   
#   
#   
#   #   if (exists("foundImgs") & exists("imgsJustNames") )
#   cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
# }
# 
# 
# 
# ################
# 
# preprocessTransmigrating <- function(filename="",imgname="",dispMet="raster",offset=0.15,writereport=FALSE,
#                                      displayprocessing=FALSE,segmMethod="adaptive",useOpening=FALSE) #,...)
# {
#   rawimg <- readImage(filename,type="jpeg")
#   
#   if (displayprocessing)    showMe(rawimg)
#   
#   flo = makeBrush(3, shape='disc', step=FALSE)^2
#   flo <- flo/sum(flo)
#   
#   adasmooththre <- thresh(filter2(rawimg,flo),w=15,h=15,offset=0.15)
#   adathre <- thresh(rawimg,w=15,h=15,offset=0.1)
#   otsuthre <- rawimg > oootsu(rawimg)
#   kittlerthre <- rawimg > kittykitty(rawimg)
#   
#   threshComparison <- EBImage::combine(rawimg,adathre,adasmooththre,otsuthre,kittlerthre,along=2)
#   # if needed with a step of opening/closing & co
#   kern <- makeBrush(size=3,shape="disc")
#   
#   if(segmMethod=="adaptiveSmoothed")
#     tosegm <- adasmooththre 
#   
#   if(segmMethod=="adaptive")
#     tosegm <- adathre # possible to choose others
#   
#   if(segmMethod=="otsu")
#     tosegm <- otsuthre # possible to choose others
#   
#   if(segmMethod=="kittler")
#     tosegm <- kittlerthre # possible to choose others
#   #   
#   if(useOpening)
#   {
#     segm <- bwlabel(fillHull(opening(tosegm,kern)))
#   } else
#     segm <- bwlabel(fillHull(tosegm))
#   
#   
#   if (displayprocessing)    showMe(segm/max(segm))
#   
#   # first round painting all objects
#   
#   obj <- paintObjects(segm,rgbImage(green=rawimg),col='#ff00ff')
#   if (displayprocessing)    showMe(obj)
#   
#   # filtering on the red channel
#   shapeAndMomfeats <- as.data.frame(computeFeatures(segm,rawimg,methods.ref="computeFeatures.basic",xname="cell"))
#   allAreas <- shapeAndMomfeats$cell.0.s.area
#   indexes <- 1:nrow(shapeAndMomfeats)
#   enoughAreas <- which((allAreas > 2) & (allAreas < 1000))
#   notEnoughAreas <- setdiff(indexes,enoughAreas)
#   notForShape <- c() # we need to define what else will be kicked out
#   filterOut <- union(notEnoughAreas,notForShape)
#   
#   # put to zero what is filtered out as not interesting
#   elab <- segm
#   for(i in filterOut)
#   {
#     elab[elab==i] <- 0
#   }
#   
#   cells <- paintObjects(elab,rgbImage(green=rawimg),col='#ff00ff')
#   cfr <- EBImage::combine(obj,cells,along=2)
#   # with filtering
#   if (displayprocessing)    showMe(cfr)
#   
#   
#   
#   # compare one above other
#   allImgs <- EBImage::combine(rgbImage(green=rawimg),obj,cells,along=2)
#   if (displayprocessing)    display(allImgs,method="raster")
#   
#   
#   ## from here on
#   # only if TRUE, produce images/reports/feature analysis table & co.
#   # using the pruned filename - removed the path
#   if (exists("foundImgs") & exists("imgsJustNames") )
#     imgName <- imgsJustNames[match(filename,foundImgs)]
#   else 
#     imgName <- imgname
#   
#   if(writereport > 0)
#   {
#     writeImage(rgbImage(green=segm),file=paste(processingFolder,"/segmentedImages/segmented_",imgName,sep=""))
#     
#     writeImage(obj,file=paste(processingFolder,"/paintedObjects/obj_",imgName,sep=""))
#     writeImage(cells,file=paste(processingFolder,"/paintedCells/cells_",imgName,sep=""))
#     
#     
#     if (displayprocessing)  display(threshComparison,method="raster",all=TRUE)
#     pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
#     display(threshComparison,method="raster",all=TRUE)
#     dev.off()
#     if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
#     pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
#     display(allImgs,method="raster",all=TRUE)
#     dev.off()
#     write.table(shapeAndMomfeats,file=paste(processingFolder,"/featuresReports/feats_mom_",imgName,".tsv",sep="")
#                 ,sep="\t",quote=FALSE,row.names=T,col.names=T)
#     
#   } else
#   {
#     if (displayprocessing)  browser()
#   }
#   
#   
#   
#   if (exists("foundImgs") & exists("imgsJustNames") )
#     cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
# }
# 
# 
# 
# 
# ################
# preprocessTransmigratingPlusWatershed <- function(filename="",imgname="",dispMet="raster",offset=0.15,writereport=FALSE,
#                                                   displayprocessing=FALSE,segmMethod="adaptive",useOpening=FALSE) #,...)
# {
#   rawimg <- readImage(filename,type="jpeg")
#   
#   if (displayprocessing)    showMe(rawimg)
#   
#   flo = makeBrush(3, shape='disc', step=FALSE)^2
#   flo <- flo/sum(flo)
#   
#   adasmooththre <- thresh(filter2(rawimg,flo),w=15,h=15,offset=0.15)
#   adathre <- thresh(rawimg,w=15,h=15,offset=0.1)
#   otsuthre <- rawimg > oootsu(rawimg)
#   kittlerthre <- rawimg > kittykitty(rawimg)
#   
#   ################# RE_UNCOMMENT? ############ threshComparison <- EBImage::combine(rawimg,adathre,adasmooththre,otsuthre,kittlerthre,along=2)
#   # if needed with a step of opening/closing & co
#   kern <- makeBrush(size=3,shape="disc")
#   
#   if(segmMethod=="adaptiveSmoothed")
#     tosegm <- adasmooththre 
#   
#   if(segmMethod=="adaptive")
#     tosegm <- adathre # possible to choose others
#   
#   if(segmMethod=="otsu")
#     tosegm <- otsuthre # possible to choose others
#   
#   if(segmMethod=="kittler")
#     tosegm <- kittlerthre # possible to choose others
#   #   
#   if(useOpening)
#   {
#     segm <- bwlabel(fillHull(opening(tosegm,kern)))
#   } else
#     segm <- bwlabel(fillHull(tosegm))
#   
#   
#   
#   dmthre <- distmap(tosegm)
#   wsthre <- watershed(dmthre)
#   
#   
#   segm <- wsthre
#   buildingUp <- rawimg
#   for(obj in 1:max(segm))
#   {
#     
#     elab_singleObject <- segm
#     elab_singleObject[segm!=obj]<- 0  
#     buildUp <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp,col=colors()[52+obj])
#     buildingUp <- buildUp
#   }
#   
#   watershedObjs <- buildingUp
#   
#   
#   
#   
#   if (displayprocessing)    showMe(segm/max(segm))
#   
#   # first round painting all objects
#   
#   obj <- paintObjects(segm,rgbImage(green=rawimg),col='#ff00ff')
#   if (displayprocessing)    showMe(obj)
#   
#   # filtering on the red channel
#   shapeAndMomfeats <- as.data.frame(computeFeatures(segm,rawimg,methods.ref="computeFeatures.basic",xname="cell"))
#   allAreas <- shapeAndMomfeats$cell.0.s.area
#   indexes <- 1:nrow(shapeAndMomfeats)
#   enoughAreas <- which((allAreas > 2) & (allAreas < 1000))
#   notEnoughAreas <- setdiff(indexes,enoughAreas)
#   notForShape <- c() # we need to define what else will be kicked out
#   filterOut <- union(notEnoughAreas,notForShape)
#   
#   # put to zero what is filtered out as not interesting
#   elab <- segm
#   for(i in filterOut)
#   {
#     elab[elab==i] <- 0
#   }
#   
#   cells <- paintObjects(elab,rgbImage(green=rawimg),col='#ff00ff')
#   ################# RE_UNCOMMENT? ############ cfr <- EBImage::combine(obj,cells,along=2)
#   # with filtering
#   ################# RE_UNCOMMENT? ############ if (displayprocessing)    showMe(cfr)
#   
#   ######## NEW
#   obj <- watershedObjs
#   cells <- watershedObjs
#   
#   
#   # compare one above other
#   ################# RE_UNCOMMENT? ############   allImgs <- EBImage::combine(rgbImage(green=rawimg),obj,cells,along=2)
#   ################# RE_UNCOMMENT? ############   if (displayprocessing)    display(allImgs,method="raster")
#   
#   
#   ## from here on
#   # only if TRUE, produce images/reports/feature analysis table & co.
#   
#   # very very RAW!!!
#   shatteredPieces <- strsplit(filename,"/")[[1]]
#   imgName <- shatteredPieces[length(shatteredPieces)]
#   
#   if(writereport > 0)
#   {
#     writeImage(rgbImage(green=segm),file=paste(processingFolder,"/segmentedImages/segmented_",imgName,sep=""))
#     
#     writeImage(obj,file=paste(processingFolder,"/paintedObjects/obj_",imgName,sep=""))
#     writeImage(cells,file=paste(processingFolder,"/paintedCells/cells_",imgName,sep=""))
#     
#     
#     ################# RE_UNCOMMENT? ############if (displayprocessing)  display(threshComparison,method="raster",all=TRUE)
#     ################# RE_UNCOMMENT? ############ pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
#     ################# RE_UNCOMMENT? ############ display(threshComparison,method="raster",all=TRUE)
#     ################# RE_UNCOMMENT? ############ dev.off()
#     ################# RE_UNCOMMENT? ############if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
#     ################# RE_UNCOMMENT? ############ pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
#     ################# RE_UNCOMMENT? ############ display(allImgs,method="raster",all=TRUE)
#     ################# RE_UNCOMMENT? ############  dev.off()
#     write.table(shapeAndMomfeats,file=paste(processingFolder,"/featuresReports/feats_mom_",imgName,".tsv",sep="")
#                 ,sep="\t",quote=FALSE,row.names=T,col.names=T)
#     
#   } else
#   {
#     if (displayprocessing)  browser()
#   }
#   
#   
#   
#   #   if (exists("foundImgs") & exists("imgsJustNames") )
#   cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
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
# ###############
