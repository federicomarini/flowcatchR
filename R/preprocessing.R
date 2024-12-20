#' Preprocessing function for `Frames` objects
#' 
#' `Frames` objects are processed according to the chosen set of parameters. Many of them refer directly to 
#' existing `EBImage` functions, please see the corresponding help for additional information
#'  
#' @param frames A `Frames` object
#' @param brush.size Size in pixels of the brush to be used for initial smoothing 
#' (low-pass filtering)
#' @param brush.shape Shape of the brush to be used for initial smoothing (low-pass 
#' filtering)
#' @param at.offset Offset to be used in the adaptive thresholding step - see also [EBImage::thresh()]. As an
#' alternative thresholding method, see also [EBImage::otsu()] in the `EBImage` package.
#' @param at.wwidth Width of the window for the adaptive thresholding step - see also [EBImage::thresh()]. As an
#' alternative thresholding method, see also [EBImage::otsu()] in the `EBImage` package.
#' @param at.wheight Height of the window for the adaptive thresholding step - see also [EBImage::thresh()]. As an
#' alternative thresholding method, see also [EBImage::otsu()] in the `EBImage` package.
#' @param kern.size Size in pixels of the kernel used for morphological operations - e.g., opening, which is an erosion followed by a dilation, and closing which is a dilation followed by an erosion - see also [EBImage::opening()], [EBImage::closing()]
#' @param kern.shape Shape of the kernel used for morphological operations
#' @param ws.tolerance Tolerance allowed in performing the watershed-based segmentation (see also [EBImage::watershed()])
#' @param ws.radius Radius for the watershed-based segmentation (see also [EBImage::watershed()])
#' @param displayprocessing Logical, whether to display intermediate steps while performing preprocessing. Dismissed currently, it could increase runtime a lot
#' @param ... Arguments to be passed to methods
#' 
#' @return A `Frames` object, whose frame images are the preprocessed versions of the input images
#' 
#' @examples
#' data("MesenteriumSubset")
#' preprocess.Frames(channel.Frames(MesenteriumSubset,"red"))
#' 
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
#' 
#' @export
preprocess.Frames <- function(frames,
                              brush.size=3,
                              brush.shape="disc",
                              at.offset=0.15,
                              at.wwidth=10,
                              at.wheight=10,
                              kern.size=3,
                              kern.shape="disc",
                              ws.tolerance=1,
                              ws.radius=1,
                              displayprocessing=FALSE,
                              ...) # for the single channel images/for one channel of multi-channel images
{
  if ( frames@channel=="all" && frames@colormode==2L ) # it is ok on a grayscale Frames object
    stop("Please select a channel to work on, run channel.Frames on the Frames object you provided")
  
  flo <- makeBrush(brush.size, brush.shape, step=FALSE)^2
  flo <- flo / sum(flo)
  # imageData(frames) is storing the raw Images
  thresh_img <- thresh(filter2(frames,flo),w=at.wwidth,h=at.wheight,offset=at.offset)
  
  # if needed with a step of smoothing & co (operations of opening,...)
  kern <- makeBrush(size=kern.size,shape=kern.shape)
  
  distmap_thre <- distmap(thresh_img)
  watershed_thre <- watershed(distmap_thre,tolerance=ws.tolerance,ext=ws.radius) 

  return(watershed_thre)
}





#' Extracts particles from the images of a `Frames` object. 
#' 
#'  
#' @param raw.frames A `Frames` object with the raw images (mandatory)
#' @param binary.frames A `Frames` object with preprocessed images (optional, if not provided gets produced with standard default parameters)
#' @param channel Character string. The channel to perform the operations on. Can be `red`, `green` or `blue`
#' @param BPPARAM a `MulticoreParam` object, used to control the performances inside the `BiocParallel` call to process 
#' frames in parallel by taking advantage of the computing infrastructure available
#' 
#' @return A `ParticleSet` object, containing all detected particles for each frame
#' 
#' @examples
#' data("MesenteriumSubset")
#' 
#' @importFrom BiocParallel bplapply bpparam
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2015
particles <- function(raw.frames,
                      binary.frames = NULL,
                      channel = NULL,
                      BPPARAM = bpparam()) 
{
  if (raw.frames@channel == "all" && is.null(channel))
    stop("Please select one channel to work on. Choose one among 'red','green' and 'blue'")
  if (raw.frames@channel == "all" && !is.null(channel))
    raw.frames <- channel.Frames(raw.frames, mode = channel)
  if (missing(channel)) 
    channel <- raw.frames@channel
  if (raw.frames@channel != "all" && channel != raw.frames@channel)
    stop("You are selecting to work on a channel not stored in your current Frames object")
  if (!is.null(binary.frames) && (raw.frames@channel != binary.frames@channel))
    stop("Your raw.frames and binary.frames objects store data related to different channels")
  if (missing(binary.frames)) {
    cat("You did not provide a preprocessed Frames object alongside with the raw set of frames.\n")
    cat("Don't worry, the raw Frames object object will be first preprocessed with a set of default parameter.\n")
    cat("You can always change them afterwards if they do not fit to your scenario.\n")
    binary.frames <- preprocess.Frames(raw.frames)
  }
  if (length(raw.frames) != length(binary.frames)) {
    stop("Raw and preprocessed Frames objects have different number of frames!")
  } else {
    cat("Computing features in parallel...\n")
  }
  
  out <- ParticleSet(channel = binary.frames@channel)
  
  out@.Data <- bplapply(1:length(raw.frames),
                        FUN = function(arg,raw.frames,binary.frames){
                          library("EBImage")
                          segmImg <- getFrame(binary.frames, arg)
                          rawImg <- getFrame(raw.frames, arg)
                          imgFeatures <- as.data.frame(EBImage::computeFeatures(segmImg,
                                                                                rawImg, xname = "cell"))
                          imgFeatures$shapeFactor <- (imgFeatures$cell.0.s.perimeter)^2 / (4 * pi * imgFeatures$cell.0.s.area)
                          imgFeatures
                        },
                        raw.frames = raw.frames,
                        binary.frames = binary.frames
                        )
  #   else {
  #     cat("son in the else")
  #     out[[arg]] <- imgFeatures
  #   }
  
  if (!is.null(dimnames(binary.frames))) {
    names(out@.Data) <- dimnames(binary.frames)[[3]]
  }
  
  
  
  
  
  
  cat("Done!\n")
  return(out)
}
# particles <- function(raw.frames,
#                       binary.frames=NULL,
#                       channel=NULL  
# )
# {
#   ## perform preliminary checks on the object(s)
#   # if still storing all channels, select one
#   if(raw.frames@channel == "all" && is.null(channel))
#     stop("Please select one channel to work on. Choose one among 'red','green' and 'blue'")
#   if(raw.frames@channel == "all" && !is.null(channel))
#     raw.frames <- channel.Frames(raw.frames,mode = channel)
#   if ( missing(channel) ) # stop("Please provide a channel name to process")
#     channel <- raw.frames@channel
#   if(raw.frames@channel != "all" && channel != raw.frames@channel)
#     stop("You are selecting to work on a channel not stored in your current Frames object")
#   if(!is.null(binary.frames) && (raw.frames@channel != binary.frames@channel))
#     stop("Your raw.frames and binary.frames objects store data related to different channels")
#   
#   
#   if(missing(binary.frames)){
#     cat("You did not provide a preprocessed Frames object alongside with the raw set of frames.\n")
#     cat("Don't worry, the raw Frames object object will be first preprocessed with a set of default parameter.\n")
#     cat("You can always change them afterwards if they do not fit to your scenario.\n")
#     binary.frames = preprocess.Frames(raw.frames)
#   }
#   if ( length.Frames(raw.frames) != length.Frames(binary.frames) ) {
#     stop("Raw and preprocessed Frames objects have different number of frames!")
#   } else {
#     cat("Computing features...\n")
#   }
#   
#   # returns a particle list - not linked yet
#   out <- ParticleSet(channel = binary.frames@channel)
#   
#   for(i in 1:length(raw.frames))  {
#     segmImg <- getFrame(binary.frames, i)
#     rawImg <- getFrame(raw.frames, i)
#     imgFeatures <- as.data.frame(EBImage::computeFeatures(segmImg,rawImg,xname="cell"))
#     imgFeatures$shapeFactor <- (imgFeatures$cell.0.s.perimeter)^2 / (4*pi*imgFeatures$cell.0.s.area)
#     
#     ## keep maybe an additional if to see if these are available? TODO
#     # with the locations now saved as names
#     if(!is.null(dimnames(binary.frames))) {
#       out[[dimnames(binary.frames)[[3]][i]]] <- imgFeatures
#     } else {
#       out[[i]] <- imgFeatures
#     }
#     
#   }
#   cat("Done!\n")
#   return(out)
# }






#' Cut borders of a `Frames` object
#' 
#' Performs cropping on the `Frames` object, selecting how many pixels should be cut on each side
#' 
#' Cropping can be performed with careful choice of all cutting sides, or cropping a single value from
#' all sides
#' 
#' @param frames An input `Frames` object
#' @param cutLeft Amount of pixels to be cut at the side
#' @param cutRight Amount of pixels to be cut at the side
#' @param cutUp Amount of pixels to be cut at the side
#' @param cutDown Amount of pixels to be cut at the side
#' @param cutAll Amount of pixels to be cut at all sides. Overrides the single side values
#' @param testing Logical, whether to just test the cropping or to actually perform it. Default set to `FALSE`
#' @param ... Arguments to be passed to [EBImage::display()] (e.g. setting the `method` argument)
#' 
#' @return A `Frames` object, with cropped frames in the `image` slot
#' 
#' @examples 
#' data("MesenteriumSubset")
#' crop.Frames(MesenteriumSubset)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
crop.Frames <- function(frames,
                        cutLeft=5,cutRight=5,cutUp=5,cutDown=5,
                        cutAll=0,
                        testing=FALSE,
                        ...) {
  if (cutAll > 0) cutLeft <- cutRight <- cutUp <- cutDown <- cutAll
  
  img <- if (isTRUE(testing)) getFrame(frames, 1, "render") else frames
  
  img <- asub(img, idx =
                list(cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown)), dims = c(1, 2))
  
  if (isTRUE(testing)) display(img, ...)
  
  return(img)
}



#' Rotates all images in a `Frames` object 
#' 
#' Rotation is performed exploiting the rotate function of the `EBImage` package. Could be automated if support for coordinate/pixel interaction is included
#' 
#' @param frames A `Frames` object
#' @param angle The rotation angle (clockwise) specified in degrees
#' @param testing Logical, whether to just test the rotation or to actually perform it. Default set to `FALSE`
#'  
#' @return A `Frames` object containing the rotated frames
#' 
#' @examples 
#' data("MesenteriumSubset")
#' rotate.Frames(MesenteriumSubset,angle = 40)
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
rotate.Frames <- function(frames,
                          angle,
                          testing=FALSE)
{
  if (!testing) {
    y <- frames
    y <- rotate(y, angle = angle) # , output.origin=output.origin, output.dim=output.dim) not required anymore after EBImage 4.9.13
    return(y)
  } else {
    # just check
    
    display(rotate(frames, angle = angle)) # , output.origin=output.origin, output.dim=output.dim)) no more required after EBImage 4.9.13
    invisible()
  }
}
