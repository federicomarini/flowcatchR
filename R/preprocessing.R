# #' Cut borders of a \code{FrameList} object
# #' 
# #' Performs cropping on the \code{FrameList} object, selecting how many pixels should be cut on each side
# #' 
# #' Cropping can be performed with careful choice of all cutting sides, or cropping a single value from
# #' all sides
# #' 
# #' @param x An input \code{FrameList} object
# #' @param cutLeft Amount of pixels to be cut at the side
# #' @param cutRight Amount of pixels to be cut at the side
# #' @param cutUp Amount of pixels to be cut at the side
# #' @param cutDown Amount of pixels to be cut at the side
# #' @param cutAll Amount of pixels to be cut at all sides. Overrides the single side values
# #' @param testing Logical, whether to just test the cropping or to actually perform it. Default set to \code{FALSE}
# #' @param ... Arguments to be passed to methods
# #' 
# #' @return A \code{FrameList} object, with cropped frames in the \code{image} slot
# #' 
# #' @examples 
# #' data("MesenteriumSubset")
# #' cut(MesenteriumSubset)
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# cut.FrameList <- function(x,
#                           cutLeft=5,cutRight=5,cutUp=5,cutDown=5,
#                           cutAll=0,
#                           testing=FALSE,
#                           ...)
# {
#   out <- vector(length(x),mode="list")
#   class(out) <- c("FrameList",class(out))
#   if(cutAll > 0)
#   {
#     cutLeft <- cutRight <- cutUp <- cutDown <- cutAll
#   }
#   
#   if(!testing)
#   {
#     for(i in 1:length(x))
#     {
#       img <- x[[i]]$image
#       if (numberOfFrames(img)==3)
#         cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
#       else
#         cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown)]
#       
#       out[[i]]$image <- cutoutImg
#       out[[i]]$location <- NA # it is modified from an existing object -> maybe provide the name of the object it got created from?
#     }
#     return(out)
#   } else {
#     # just check on one image, the first one
#     img <- x[[1]]$image
#     cutoutImg <- img[cutLeft:(dim(img)[1]-cutRight),cutUp:(dim(img)[2]-cutDown),]
#     display(cutoutImg)
#     return(cutoutImg)
#   }
# }
# 
# 
# 
# #' Rotates all images in a \code{FrameList} object 
# #' 
# #' Rotation is performed exploiting the rotate function of the \code{EBImage} package. Could be automated if support for coordinate/pixel interaction is included
# #' 
# #' @param framelist A \code{FrameList} object
# #' @param rotAngle The rotation angle (clockwise) specified in degrees
# #' @param testing Logical, whether to just test the rotation or to actually perform it. Default set to \code{FALSE}
# #' @param output.origin A vector of 2 numbers indicating the dimension of the output image, as in the rotate function
# #' @param output.dim A vector of 2 numbers indicating the output coordinates of the origin in pixels, as in the \code{rotate} function
# #'  
# #' @return A \code{FrameList} object containing the rotated frames
# #' 
# #' @examples 
# #' data("MesenteriumSubset")
# #' rotate.FrameList(MesenteriumSubset,rotAngle = 40)
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# rotate.FrameList <- function(framelist,
#                              rotAngle=0,
#                              testing=FALSE,
#                              output.origin=c(dim(framelist[[i]]$image)[1]/3,dim(framelist[[i]]$image)[2]/3),
#                              output.dim=c(dim(framelist[[i]]$image)[1]*1.5,dim(framelist[[i]]$image)[2]*1.5))
# {
#   out <- vector(length(framelist),mode="list")
#   class(out) <- c("FrameList",class(out))
#     
#   if(!testing)
#   {
#     for(i in 1:length(framelist))
#     {
#       img <- framelist[[i]]$image
#       rotatedImg <- rotate(img,rotAngle,
#                            output.origin=output.origin,
#                            output.dim=output.dim)
#       out[[i]]$image <- rotatedImg
#       out[[i]]$location <- NA
#       
#     } 
#     return(out)
#   } else {
#     # just check on one image, the first one
#     img <- framelist[[1]]$image
#     rotatedImg <- rotate(img,rotAngle,
#                          output.origin=output.origin,
#                          output.dim=output.dim)
#     display(rotatedImg)              
#     return(rotatedImg)
#   }  
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
# 
# 
# #' Preprocessing images
# #' 
# #' Can be applied to \code{FrameList} or \code{ChannelsFrameList} objects. \code{ChannelsFrameList} are then subset to the chosen channel,
# #' and the method \code{preprocess.FrameList} is then applied, with its set of parameters
# #'  
# #' @param x A \code{FrameList} or a \code{ChannelsFrameList} object
# #' @param ... Arguments to be passed to methods, such as channel and/or preprocessing parameters
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# preprocess <- function(x,...)
# {
#   UseMethod("preprocess")
# }
# 
# 
# 
# #' Preprocessing function for \code{ChannelsFrameList} objects
# #' 
# #' \code{ChannelsFrameList} are then subset to the chosen channel, and the method \code{preprocess.FrameList} is then applied, with its set of parameters
# #'  
# #' @param x A \code{ChannelsFrameList} object
# #' @param channel Character string. The channel to perform the operations on. Can be \code{red}, \code{green} or \code{blue}
# #' @param ... Arguments to be passed to methods
# #' 
# #' @return A \code{FrameList} object, whose frame images are the preprocessed versions of the input images
# #' 
# #' @examples
# #' data("MesenteriumSubset")
# #' preprocess(channels(MesenteriumSubset),channel = "red")
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# preprocess.ChannelsFrameList <- function(x,
#                                          channel="",
#                                          ...) 
# {
#   switch(channel,
#          red={
#            cat("Preprocessing the red channel...\n")
#            out <- preprocess.FrameList(x[[1]])
#            }, 
#          green={
#            cat("Preprocessing the green channel...\n")
#            out <- preprocess.FrameList(x[[2]])
#          },
#          blue={
#            cat("Preprocessing the blue channel...\n")
#            out <- preprocess.FrameList(x[[3]])
#          },
#          stop("You did not choose any of the value for the channel - allowed values= red | green | blue")
#   )
#   return(out)
# }
# 
# 
# #' Preprocessing function for \code{FrameList} objects
# #' 
# #' \code{FrameList} objects are processed according to the chosen set of parameters. Many of them refer directly to 
# #' existing \code{EBImage} functions, please see the corresponding help for additional information
# #'  
# #' @param x A \code{FrameList} object
# #' @param brush.size Size in pixels of the brush to be used for initial smoothing
# #' @param brush.shape Shape of the brush to be used for initial smoothing
# #' @param at.offset Offset to be used in the adaptive thresholding step
# #' @param at.wwidth Width of the window for the adaptive thresholding step
# #' @param at.wheight Height of the window for the adaptive thresholding step
# #' @param kern.size Size in pixels of the kernel used for morphological operations
# #' @param kern.shape Shape of the kernel used for morphological operations
# #' @param ws.tolerance Tolerance allowed in performing the watershed-based segmentation
# #' @param ws.radius Radius for the watershed-based segmentation
# #' @param displayprocessing Logical, whether to display intermediate steps while performing preprocessing. Dismissed currently, it could increase runtime a lot
# #' @param ... Arguments to be passed to methods
# #' 
# #' @return A \code{FrameList} object, whose frame images are the preprocessed versions of the input images
# #' 
# #' @examples
# #' data("MesenteriumSubset")
# #' platelets.framelist <- channels(MesenteriumSubset)$red
# #' preprocess(platelets.framelist)
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# preprocess.FrameList <- function(x,
#                                  brush.size=3,
#                                  brush.shape="disc",
#                                  at.offset=0.15,
#                                  at.wwidth=10,
#                                  at.wheight=10,
#                                  kern.size=3,
#                                  kern.shape="disc",
#                                  ws.tolerance=1,
#                                  ws.radius=1,
#                                  displayprocessing=FALSE,
#                                  ...) # for the single channel images/for one channel of multi-channel images
# {
# #   cat("do this - processing the single channel")
#   out <- vector(length(x),mode="list")
#   class(out) <- c("FrameList",class(out))
#   
#   for(i in 1:length(x))
#   {
#     rawimg <- x[[i]]$image
# #     colorMode(rawimg) <- Grayscale
#     
#     flo = makeBrush(brush.size, brush.shape, step=FALSE)^2
#     flo <- flo/sum(flo)
#     
#     thresh_img <- thresh(filter2(rawimg,flo),w=at.wwidth,h=at.wheight,offset=at.offset)
#     
#     # if needed with a step of smoothing & co (operations of opening,...)
#     kern <- makeBrush(size=kern.size,shape=kern.shape)
#     
#     distmap_thre <- distmap(thresh_img)
#     watershed_thre <- watershed(distmap_thre,tolerance=ws.tolerance,ext=ws.radius) 
#   
#     out[[i]]$image <- watershed_thre
#     out[[i]]$channel <- x[[i]]$channel
#     out[[i]]$location <- x[[i]]$location  # or maybe call it location raw? 
#   }
#   return(out)
# }
# 
# 
# 
# 
# 
# 
# #' Extracts particles from the images of a \code{FrameList} object. 
# #' 
# #'  
# #' @param framelistRaw A \code{FrameList} object with the raw images (mandatory)
# #' @param framelistPreprocessed A \code{FrameList} object with preprocessed images (optional, if not provided gets produced with standard default parameters)
# #' @param channel Character string. The channel to perform the operations on. Can be \code{red}, \code{green} or \code{blue}
# #' 
# #' @return A \code{ParticleList} object, containing all detected particles for each frame
# #' 
# #' @examples
# #' data("MesenteriumSubset")
# #' platelets.framelist <- channels(MesenteriumSubset)$red
# #' platelets.preprocessed <- preprocess(platelets.framelist)
# #' particles.platelets <- particles(platelets.framelist, platelets.preprocessed)
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# particles <- function(framelistRaw,
#                       framelistPreprocessed=NULL,
#                       channel=""  # if we provide the channelsFrameList as input 
#                       )
# {
#   if(!is(framelistRaw,"FrameList") && !is(framelistRaw,"ChannelsFrameList"))
#   {
#     stop("You need to provide at least a FrameList/channelsFrameList object as input!")
#   }
#   
#   
#   if(is.null(framelistPreprocessed))
#   {
#     cat("You did not provide a preprocessed FrameList alongside with the raw set of frames.\n")
#     cat("Don't worry, the raw FrameList object will be first preprocessed with a set of default parameter.\n")
#     cat("You can always change them afterwards if they do not fit to your scenario.\n")
#     if(is(framelistRaw,"ChannelsFrameList"))
#     {
# #       framelistRaw <- framelistRaw[[channel]]
#       framelistPreprocessed <- preprocess.ChannelsFrameList(framelistRaw[[channel]])
#     } else {
#       framelistPreprocessed <- preprocess.FrameList(framelistRaw)
#     }
#   }
#   
#   # check that both input framelists have same length
#   if(length(framelistRaw) != length(framelistPreprocessed) )
#   {
#     stop("FrameList objects have different lengths!")
#   } else {
#     cat("Computing features...\n")
#   }
#   
#   # returns a particle list - not linked yet
#   
#   out <- vector(length(framelistRaw),mode="list")
#   class(out) <- c("ParticleList",class(out))
#   
#   for(i in 1:length(framelistRaw))
#   {
#     segmImg <- framelistPreprocessed[[i]]$image
#     rawImg <- framelistRaw[[i]]$image
#     
#     imgFeatures <- as.data.frame(computeFeatures(segmImg,rawImg,xname="cell"))
#     imgFeatures$shapeFactor <- (imgFeatures$cell.0.s.perimeter)^2 / (4*pi*imgFeatures$cell.0.s.area)
#     
#     out[[i]]$particles <- imgFeatures
#     out[[i]]$imgSource <- framelistPreprocessed[[i]]$location
#     out[[i]]$channel <- framelistPreprocessed[[i]]$channel
#   }
#   cat("Done!\n")
#   return(out)
# }
# 
# 
# # particles2 <- function(framelistRaw,
# #                       framelistPreprocessed=NULL,
# #                       channel=""  # if we provide the channelsFrameList as input 
# # )
# # {
# #   if(!is(framelistRaw,"FrameList") && !is(framelistRaw,"ChannelsFrameList"))
# #   {
# #     stop("You need to provide at least a FrameList/channelsFrameList object as input!")
# #   }
# #   if(is.null(framelistPreprocessed))
# #   {
# #     cat("You did not provide a preprocessed FrameList alongside with the raw set of frames.\n")
# #     cat("Don't worry, the raw FrameList object will be first preprocessed with a set of default parameter.\n")
# #     cat("You can always change them afterwards if they do not fit to your scenario.\n")
# #     if(is(framelistRaw,"ChannelsFrameList"))
# #     {
# #       #       framelistRaw <- framelistRaw[[channel]]
# #       framelistPreprocessed <- preprocess.ChannelsFrameList(framelistRaw[[channel]])
# #     } else {
# #       framelistPreprocessed <- preprocess.FrameList(framelistRaw)
# #     }
# #   }
# #   
# #   # check that both input framelists have same length
# #   if(length(framelistRaw) != length(framelistPreprocessed) )
# #   {
# #     stop("FrameList objects have different lengths!")
# #   } else {
# #     cat("Computing features...\n")
# #   }
# #   
# #   # returns a particle list - not linked yet
# #   
# #   
# #   
# #   
# #   
# #   ## here i should simplify the list
# #   out <- vector(length(framelistRaw),mode="list")
# #   class(out) <- c("ParticleList",class(out))
# #   
# #   for(i in 1:length(framelistRaw))
# #   {
# #     segmImg <- framelistPreprocessed[[i]]$image
# #     rawImg <- framelistRaw[[i]]$image
# #     
# #     imgFeatures <- as.data.frame(computeFeatures(segmImg,rawImg,xname="cell"))
# #     imgFeatures$shapeFactor <- (imgFeatures$cell.0.s.perimeter)^2 / (4*pi*imgFeatures$cell.0.s.area)
# #     
# #     out[[i]]$particles <- imgFeatures
# #     out[[i]]$imgSource <- framelistPreprocessed[[i]]$location
# #     out[[i]]$channel <- framelistPreprocessed[[i]]$channel
# #   }
# #   cat("Done!\n")
# #   return(out)
# # }
# 
# 
# 
# 
# 
# #' Performs filtering on a \code{ParticleList} object
# #' 
# #' According to parameters of interests, such as size, eccentricity/shape, filters out the particles that do not 
# #' satisfy the indicated requirements
# #' 
# #' @param particlelist A \code{ParticleList} object. A \code{LinkedParticleList} object can also be provided as input, yet the returned object will be a \code{ParticleList} object that needs to be linked again 
# #' @param min.area Size in pixels of the minimum area needed to detect the object as a potential particle of interest
# #' @param max.area Size in pixels of the maximum area allowed to detect the object as a potential particle of interest
# #'  
# #' @return A \code{ParticleList} object
# #' 
# #' @examples
# #' data("candidate.platelets")
# #' selected.platelets <- select.particles(candidate.platelets, min.area = 5)
# #' selected.platelets
# #' 
# #' @export
# #' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
# select.particles <- function(particlelist,
#                              min.area = 1,
#                              max.area = 1000 #, # and others of interest, for example
#                             #shapeThreshold = 0.5,
#                             #eccentricityThreshold = 0.7 # currently not so efficient with so small particles available in the images!!
#                             )
# {
#   # returns a particle list - not linked yet
#   out <- vector(length(particlelist),mode="list")
#   if(is(particlelist,"linkedParticleList"))
#   {
#     class(out) <- c("ParticleList",class(out))
#     cat("Warning, you are filtering particles that were previously linked by tracking them - reperform the linking afterwards.\n")
#   } else {
#     if(is(particlelist,"ParticleList")) 
#     {
#       class(out) <- c("ParticleList",class(out))
#       cat("Filtering the particles...\n")
#     } else {
#       stop("You need to provide a ParticleList object as input for select.particles!\n")
#     }
#   }
#   
#   
#   for(i in 1:length(particlelist))
#   {
#     candidateParticles <- particlelist[[i]]$particles
#     
#     nobjects <- nrow(candidateParticles)
#     candidateParticles$shapeFactor <- (candidateParticles$cell.0.s.perimeter)^2 / (4*pi*candidateParticles$cell.0.s.area)
#     
#     notBecauseOfArea <- c()
#     notBecauseOfEccen <- c()
#     notBecauseOfShape <- c()
#     
#     notBecauseOfArea <- which( (candidateParticles$cell.0.s.area < min.area) | (candidateParticles$cell.0.s.area > max.area) )
# #     notBecauseOfEccen <- which(candidateParticles$cell.0.m.eccentricity > eccenThreshold)
#     
#     leftOut <- unique(c(notBecauseOfArea,notBecauseOfEccen)) #, not because of ....
#     if(length(leftOut)!=0)#     if(!is.null(leftOut))
#     {
#       filteredParticles <- candidateParticles[-leftOut,]
#     } else {
#       filteredParticles <- candidateParticles
#     }
#       #       cat("Had", nrow(framestats), "\t; Kept",nrow(candidates),"\n")
#     
#     
#     out[[i]]$particles <- filteredParticles
#     out[[i]]$imgSource <- particlelist[[i]]$imgSource
#     out[[i]]$channel <- particlelist[[i]]$channel
#   }
#   return(out)
# }
# 
# 
# 
