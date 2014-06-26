
#' combine.preprocessedFrameList
#' 
#' Combines the information from a raw FrameList object and the corresponding preprocessed one
#' 
#' All objects are painted with a unique colour - for sake of speed
#'  
#' @param rawframelist A FrameList object containing the raw images
#' @param preprocessedframelist A FrameList object with the preprocessed versions of the images (e.g. segmented)
#' 
#' @return A FrameList object, whose images are the combination of the raw images with the segmented objects drawn on them
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
combine.preprocessedFrameList <- function(rawframelist,preprocessedframelist)
{
  out <- vector("list",length(rawframelist))
  for (i in 1:length(out))
  {
    rawimg <- rawframelist[[i]]$image
    segmimg <- preprocessedframelist[[i]]$image
    
    # works well if rawimg has still all 3 frames, otherwise i guess it stays B/W
    if(length(dim(rawimg))>2)
    {
      rawWithObj <- paintObjects(segmimg,rawimg,col="yellow")
      
      
      
      # TODO a slower version that paints all objects singularly but of different colours?
      # similar to this..
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
      
    } else {
      channel <- rawframelist[[i]]$channel # read it directly from the framelist object!
      switch(channel,
             red={
               rawWithObj <- rgbImage(red=paintObjects(segmimg,rawimg))
             }, 
             green={
               rawWithObj <- rgbImage(green=paintObjects(segmimg,rawimg))
             },
             blue={
               rawWithObj <- rgbImage(blue=paintObjects(segmimg,rawimg))
             },
             stop("No channel value was stored in the appropriate slot!")
      )
    }
    out[[i]]$image <- rawWithObj
  }
  
  class(out) <- c("FrameList",class(out))
  return(out)
  
}



#' actionPaint
#'  
#' @param rawimg An Image object with the raw frame data
#' @param segmimg An Image object with the segmented objects
#' 
#' @return An Image object that combines raw and segmented images, with objects painted singularly with different colours
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
actionPaint <- function(rawimg,segmimg)
{
  buildingUp <- rawimg
  for(obj in 1:max(segmimg))
  {
    elab_singleObj <- segmimg
    elab_singleObj[segmimg!=obj] <- 0
    #         buildUp <- paintObjects(Image(elab_singleObject,colormode="Grayscale"),buildingUp_green,col=colors()[52+obj])
    builtUp <- paintObjects(elab_singleObj,buildingUp,col=colors()[52+obj])
    buildingUp <- builtUp
  }
  return(builtUp)
}






#' combineWcolor.preprocessedFrameList
#' 
#' Combines the information from a raw FrameList object and the corresponding preprocessed one,
#' but this time every object is painted with a different colour
#' 
#' Every object is now shown with a different colour. Care should be taken, as this function is rather slower
#' than combine.preprocessedFrameList
#'   
#' @param rawframelist A FrameList object containing the raw images
#' @param preprocessedframelist A FrameList object with the preprocessed versions of the images (e.g. segmented)
#' 
#' @return A FrameList object, whose images are the combination of the raw images with the segmented objects drawn on them,
#' painted singularly with different colours
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
combineWcolor.preprocessedFrameList <- function(rawframelist,preprocessedframelist) 
  # careful, it's kind of slower for painting all single objects separately and of different colors!
  # currently works best only when input raw image is in colorMode Color - as the colours there have actually a meaning 
  # maybe do a function actionPaint that actually is internally called here - params: rawimg, segmimg -> DONE
{
  out <- vector("list",length(rawframelist))
  
  if(length(dim(rawframelist[[1]]$image))<3)
    cat("Read the warning message!!\n")
    warning("You are trying to paint coloured cells on a single channel - info will be not so useful, try instead combine.preprocessedFrameList, it should be much faster!")
  
  for (i in 1:length(out))
  {
    rawimg <- rawframelist[[i]]$image
    segmimg <- preprocessedframelist[[i]]$image
    
    # works well if rawimg has still all 3 frames, otherwise i guess it stays B/W
    if(length(dim(rawimg))>2)
    {
      rawWithObj <- actionPaint(rawimg,segmimg)
      
      # TODO a slower version that paints all objects singularly but of different colours?
      # similar to this..
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
      
    } else {
      channel <- rawframelist[[i]]$channel
      
      switch(channel,
             red={
               rawWithObj <- rgbImage(red=actionPaint(rawimg,segmimg))
             }, 
             green={
               rawWithObj <- rgbImage(green=actionPaint(rawimg,segmimg))
             },
             blue={
               rawWithObj <- rgbImage(blue=actionPaint(rawimg,segmimg))
             },
             stop("No channel value was stored in the appropriate slot!")
      )
    }
    out[[i]]$image <- rawWithObj
  }
  
  class(out) <- c("FrameList",class(out))
  return(out)
  
}









# ################
# 
# processFeats <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
# {
#   framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
#   areas <- framestats$cell.0.s.area
#   nobjects <- nrow(framestats)
#   shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
#   #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
#   # better like this
#   # list exclusion criteriums
#   notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
#   notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
#   notBecauseOfShape <- c()
#   # more to come with higher resolution?
#   
#   # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
#   leftOut <- union(notBecauseOfArea,notBecauseOfEccen) #, not because of ....
#   candidates <- framestats[-leftOut,]
#   cat("Found",nobjects,"objects to begin with ------\n")
#   cat("Eliminated",length(notBecauseOfArea),"because of the area;",length(notBecauseOfEccen),"because of the eccentricity;",length(leftOut),"in total\n")
#   cat("Frame",j,"\t\t",nobjects-length(leftOut),"potential cells according to area were identified!\n\n")
#   # other stuff, e.g. save output to some log? # e.g. whatever is being cat'd, append it to a log file
#   # e.g.
#   # 2x2, images painted
#   # below, the histograms for the areas
#   #   
#   #   par(mfrow=c(2,2))
#   #   pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
#   #   hist(areas) 
#   #   hist(areas[areas>areaThreshold]) # & other criteria
#   
# }
# 
# 
# reallyProcessReport <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
# {
#   framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
#   areas <- framestats$cell.0.s.area
#   nobjects <- nrow(framestats)
#   shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
#   #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
#   # better like this
#   # list exclusion criteriums
#   notBecauseOfArea <- c()
#   notBecauseOfEccen <- c()
#   #   notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
#   #   notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
#   #   
#   notBecauseOfShape <- c()
#   # more to come with higher resolution?
#   
#   # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
#   leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
#   if(!is.null(leftOut))
#   {
#     candidates <- framestats[-leftOut,]
#   } else
#     candidates <- framestats
#   
#   cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
#   write.table(candidates,
#               file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
#               row.names=FALSE, sep="\t"
#   )
# }
# 
# 
# 
# 
# processFeats2atOnce <- function(idx,reportFilesFolder,objImageFolder,cellImageFolder,
#                                 areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,
#                                 folderPostprocessing="")
# {
#   featsRepList <- list.files(path=reportFilesFolder,full.names=T)
#   objImgList <- list.files(path=objImageFolder,full.names=T)
#   cellImgList <- list.files(path=cellImageFolder,full.names=T)
#   
#   # will use idx to navigate through them
#   
#   
#   framestats <- read.delim(featsRepList[idx],sep="\t",stringsAsFactors=FALSE,header=TRUE)
#   
#   areas <- framestats$cell.0.s.area
#   nobjects <- nrow(framestats)
#   shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
#   #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
#   # better like this
#   # list exclusion criteriums
#   notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
#   notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
#   notBecauseOfShape <- c()
#   # more to come with higher resolution?
#   
#   # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
#   leftOut <- union(notBecauseOfArea,notBecauseOfEccen) #, not because of ....
#   candidates <- framestats[-leftOut,]
#   cat("Eliminated",length(notBecauseOfArea),"because of the area;",length(notBecauseOfEccen),"because of the eccentricity;",length(leftOut),"in total\n")
#   cat("Frame",idx,"\t\t",nobjects-length(leftOut),"potential cells according to area were identified!\n\n")
#   # other stuff, e.g. save output to some log? # e.g. whatever is being cat'd, append it to a log file
#   # e.g.
#   # 2x2, images painted
#   # below, the histograms for the areas
#   
#   
#   pdf(file=paste(folderPostprocessing,"/image",idx,".pdf",sep="")) # to stick to the names mentioned # eventually take the substring?
#   par(mfrow=c(2,2))
#   hist(areas) 
#   hist(areas[areas>areaThreshold]) # & other criteria
#   display(readImage(objImgList[idx]),method="raster")
#   display(readImage(cellImgList[idx]),method="raster")
#   dev.off()
#   # somehow it is not displayes in the same page, dammit! -> use one figure? with EBI::combine
# }
# # for(j in feats_list_green)
# # {
# #   processFeats(j)
# # }
# # 
# # 
# # featsRepList <- list.files(path="./featuresReports_green/",full.names=T)
# # objImgList <- list.files(path="./featuresReports_green/",full.names=T)
# # celImgList <- list.files(path="./featuresReports_green/",full.names=T)
# # 
# # for(k in 1:100)
# # {
# #   processFeats2atOnce(idx=k,
# #                       reportFilesFolder="featuresReports_green/",
# #                       objImageFolder="paintedObjects_green/",
# #                       cellImageFolder="paintedCells_green/",
# #                       folderPostprocessing="postprocessing_green/")
# # }
# 
# ################
# ################ TRACKING ATTEMPTS #####################
# # start with
# # feats_list_green # for example
# 
# processReport <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
# {
#   framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
#   areas <- framestats$cell.0.s.area
#   nobjects <- nrow(framestats)
#   shapeFactor <- (framestats$cell.0.s.perimeter)^2 / (4*pi*framestats$cell.0.s.area)
#   #   usefulOnes <- data.frame(framestats,shapeFactor=shapeFactor) # moments, shape and basic # does not work in the separate functions  
#   # better like this
#   # list exclusion criteriums
#   notBecauseOfArea <- which(framestats$cell.0.s.area <= areaThreshold)
#   notBecauseOfEccen <- which(framestats$cell.0.m.eccentricity > eccenThreshold)
#   
#   notBecauseOfShape <- c()
#   # more to come with higher resolution?
#   
#   # sum(framestats$cell.0.m.eccentricity > 0.5)/nrow(framestats) # to see and adjust accordingly? 
#   leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
#   if(!is.null(leftOut))
#   {
#     candidates <- framestats[-leftOut,]
#   } else
#     candidates <- framestats
#   
#   cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
#   write.table(candidates,
#               file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
#               row.names=FALSE, sep="\t"
#   )
# }
# 
# # for(j in feats_list_green)
# # {
# #   processReport(j,folderPostprocessing="processedReports_green/",areaThreshold=3,shapeThreshold=0.5,eccenThreshold=1)
# # }
# 
