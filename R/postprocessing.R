
#' combine.preprocessedFrameList
#' 
#' Combines the information from a raw FrameList object and the corresponding preprocessed one
#' 
#' All objects are painted with a unique colour - for sake of speed
#'  
#' @param rawframelist A FrameList object containing the raw images
#' @param preprocessedframelist A FrameList object with the preprocessed versions of the images (e.g. segmented)
#' @param col A color character string, to select which color will be used for drawing the contours of the particles. If not specified, it will default according to the objects provided
#' 
#' @return A FrameList object, whose images are the combination of the raw images with the segmented objects drawn on them
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
combine.preprocessedFrameList <- function(rawframelist,preprocessedframelist,col=NULL)
{
  out <- vector("list",length(rawframelist))
  for (i in 1:length(out))
  {
    rawimg <- rawframelist[[i]]$image
    segmimg <- preprocessedframelist[[i]]$image
    
    # works well if rawimg has still all 3 frames, otherwise i guess it stays B/W
    if(length(dim(rawimg))>2)
    {
      if(is.null(col)) col <- "yellow"
      rawWithObj <- paintObjects(segmimg,rawimg,col=col)
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









