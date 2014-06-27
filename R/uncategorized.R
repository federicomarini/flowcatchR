
# to be used in combination with a binary image - segmented e.g. via watershed/other methods

#' reproducibleColorLabels
#' 
#' Color codes the labels of object masks by a random permutation
#' 
#' The labels of object masks are coloured this time in a "random but reproducible way", by setting a seed at the beginning of the function
#' 
#' @param x an Image object in Grayscale color mode or an array containing object masks
#' @param normalize Logical, if TRUE normalizes the resulting color image
#' 
#' @return An Image object with color coded objects
#' 
#' @references colorLabels in the EBImage package
#' 
#' 
#' @export
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
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


## de facto not useful, the call takes ONE COLOR!

# ## this will not work the way it is implemented now in C...
# actionPainting <- function (x, tgt, opac = c(1, 1), col = c("red", NA), thick = FALSE) 
# {
#   EBImage:::validImage(x)
#   if (colorMode(x) != Grayscale) 
#     stop("'", deparse(substitute(x), width.cutoff = 500L, 
#                       nlines = 1), "' must be in 'Grayscale' color mode")
#   if (any(dim(x)[1:2] != dim(tgt)[1:2])) 
#     stop("'x' and 'tgt' must have the same size")
#   if (getNumberOfFrames(x, "render") != getNumberOfFrames(tgt, 
#                                                           "render")) 
#     stop("'x' and 'tgt' must have the same number of render frames")
#   
#   
#   
#   
#   col = c(col, rep(NA, 3 - length(col)))
#   opac = c(opac, rep(1, 3 - length(opac)))
#   zcol = which(is.na(col))
#   col[zcol] = "white"
#   opac[zcol] = 0
#   opac = as.numeric(opac)
#   if (any(opac < 0) || any(opac > 1)) 
#     stop("all opacity values must be in the range [0,1]")
#   .Call("paintObjects", EBImage:::castImage(x), EBImage:::castImage(tgt), opac, 
#         Image(col), as.integer(thick), PACKAGE = "EBImage")
# }


# # labelCells(rawtest[,,1],protest,classes=c("yes","no"),classColours=c("red","blue"))
# labelCellsFROMCRIMAGE <- function (img, segmentedImage, classes, classColours, nblocks = 3, 
#           labeledPoints = NULL, filename = NULL, filenameImage = NULL, 
#           transformCoordinates = FALSE) 
# {
#   oldX = NULL
#   oldY = NULL
#   options(stringsAsFactors = FALSE)
#   f = computeFeatures.moment(segmentedImage)
#   print(f)
#   activateHull = FALSE
#   convHull = c()
#   xyCell = f[, c("m.cx", "m.cy")]
#   blockPosition = data.frame(stringsAsFactors = FALSE)
#   xs = floor(dim(img)[1]/nblocks)
#   ys = floor(dim(img)[2]/nblocks)
#   blockCounter = 1
#   xPos = 1
#   yPos = 1
#   for (i in 1:nblocks) {
#     xPos = 1
#     for (j in 1:nblocks) {
#       blockPosition = rbind(blockPosition, c(blockCounter, 
#                                              xPos, (xPos + xs - 1), yPos, (yPos + ys - 1)))
#       blockCounter = blockCounter + 1
#       xPos = xPos + (xs - 1)
#     }
#     yPos = yPos + (ys - 1)
#   }
#   colnames(blockPosition) = c("block", "xs", "xe", "ys", "ye")
#   classCounter = 1
#   actClass = classes[classCounter]
#   remove = FALSE
#   action = "add"
#   description = c("Key: A=add point, D=delete point, H=Hull, C=switch class, Q=Exit, R=refresh, W=Write image to file ")
#   actValues = paste("Class:", actClass, "Action:", action)
#   actBlock = 1
#   if (is.null(labeledPoints)) {
#     print("No points labled.")
#     labeledPoints = data.frame(t(rep(0, 7)), stringsAsFactors = FALSE)
#     colnames(labeledPoints) = c("index", "x", "y", "classCell", 
#                                 "xLocal", "yLocal", "block")
#   }
#   else {
#     labeledPoints = data.frame(as.matrix(labeledPoints), 
#                                stringsAsFactors = FALSE)
#     if (transformCoordinates == TRUE) {
#       labeledPointsN = labeledPoints
#       for (i in 1:nblocks) {
#         actPosition = blockPosition[blockPosition$block == 
#                                       i, ]
#         labeledPointsToTransformBlock = labeledPoints[labeledPoints$block == 
#                                                         i, ]
#         xCoordLocal = abs(actPosition$ye - actPosition$ys) - 
#           as.numeric(labeledPointsToTransformBlock$xLocal)
#         yCoordLocal = abs(actPosition$xe - actPosition$xs) - 
#           as.numeric(labeledPointsToTransformBlock$yLocal)
#         labeledPoints[labeledPoints$block == i, "xLocal"] = yCoordLocal
#         labeledPoints[labeledPoints$block == i, "yLocal"] = xCoordLocal
#       }
#     }
#   }
#   labeledPointsToPaint = labeledPoints
#   labeledPointsNoNuclei = data.frame(stringsAsFactors = FALSE)
#   if (dim(labeledPoints)[1] > 1) {
#     if (length(which(as.numeric(labeledPoints[, 1]) == 0)) > 
#           0) {
#       labeledPointsToPaint = labeledPoints[-which(as.numeric(labeledPoints[, 
#                                                                            1]) == 0), ]
#       labeledPointsNoNuclei = labeledPoints[which(as.numeric(labeledPoints[, 
#                                                                            1]) == 0), ]
#     }
#   }
#   if (dim(labeledPointsToPaint)[1] > 0) {
#     paintedNuclei = paintCells(segmentedImage, img, as.character(labeledPointsToPaint[, 
#                                                                                       4]), as.numeric(labeledPointsToPaint[, 1]), classes, 
#                                colors = classColours)
#   }
#   actPosition = blockPosition[blockPosition$block == actBlock, 
#                               ]
#   paintedNucleiN = paintedNuclei[actPosition$xs:actPosition$xe, 
#                                  actPosition$ys:actPosition$ye, ]
#   paintedNucleiN = aperm(paintedNucleiN, c(2, 1, 3))
#   plotImage(paintedNucleiN)
#   if (dim(labeledPointsNoNuclei)[1] > 0) {
#     labeledPointsNoNucleiBlock = labeledPointsNoNuclei[labeledPointsNoNuclei$block == 
#                                                          actBlock, ]
#     xCoord = as.numeric(labeledPointsNoNucleiBlock$xLocal)
#     yCoord = as.numeric(labeledPointsNoNucleiBlock$yLocal)
#     points(xCoord, yCoord, col = "red", pch = "x")
#   }
#   title(main = description, sub = actValues, col.sub = "black", 
#         cex.main = 0.7)
#   refresh = function() {
#     trainingNuclei = segmentedImage
#     trainingNuclei[, ] = 0
#     allIndices = c()
#     labeledPointsToPaint = labeledPoints
#     labeledPointsNoNuclei = data.frame(stringsAsFactors = FALSE)
#     if (dim(labeledPoints)[1] > 1) {
#       if (length(which(as.numeric(labeledPoints[, 1]) == 
#                          0)) > 0) {
#         labeledPointsToPaint = labeledPoints[-which(as.numeric(labeledPoints[, 
#                                                                              1]) == 0), ]
#         labeledPointsNoNuclei = labeledPoints[which(as.numeric(labeledPoints[, 
#                                                                              1]) == 0), ]
#       }
#     }
#     actPosition = blockPosition[blockPosition$block == actBlock, 
#                                 ]
#     print("Label nuclei")
#     paintedNucleiSmall = paintCells(segmentedImage[actPosition$xs:actPosition$xe, 
#                                                    actPosition$ys:actPosition$ye], img[actPosition$xs:actPosition$xe, 
#                                                                                        actPosition$ys:actPosition$ye, ], as.character(labeledPointsToPaint[, 
#                                                                                                                                                            4]), as.numeric(labeledPointsToPaint[, 1]), classes, 
#                                     colors = classColours)
#     print("Nuclei labeled")
#     paintedNuclei[actPosition$xs:actPosition$xe, actPosition$ys:actPosition$ye, 
#                   ] = paintedNucleiSmall
#     paintedNuclei <<- paintedNuclei
#     paintedNucleiSmall = aperm(paintedNucleiSmall, c(2, 1, 
#                                                      3))
#     plotImage(paintedNucleiSmall)
#     labeledPointsNoNucleiBlock = labeledPointsNoNuclei[labeledPointsNoNuclei$block == 
#                                                          actBlock, ]
#     points(labeledPointsNoNucleiBlock$xLocal, labeledPointsNoNucleiBlock$yLocal, 
#            col = "red", pch = "x")
#     title(main = description, sub = actValues, col.sub = "black", 
#           cex.main = 0.7)
#     if (!is.null(filename)) {
#       write.table(labeledPoints, file = filename, sep = "\t", 
#                   row.names = FALSE, quote = FALSE)
#     }
#   }
#   keydown <- function(key) {
#     if (key == "r") {
#       refresh()
#     }
#     if (key == "q") {
#       if (!is.null(filename)) {
#         write.table(labeledPoints, file = filename, sep = "\t", 
#                     row.names = FALSE, quote = FALSE)
#         print("Save")
#       }
#       print("Quit")
#       return(invisible(1))
#     }
#     if (key == "w") {
#       print("Write labeled image to file.")
#       if (!is.null(filenameImage)) {
#         for (n in 1:dim(labeledPoints)[1]) {
#           if (as.numeric(labeledPoints$index[n]) == 0) {
#             actNuclei = labeledPoints[n, ]
#             sizeNCrossU = 3
#             sizeNCrossD = 3
#             sizeNCrossR = 3
#             sizeNCrossL = 3
#             crossL = (as.numeric(actNuclei$x) - sizeNCrossL)
#             crossR = (as.numeric(actNuclei$x) + sizeNCrossR)
#             crossU = (as.numeric(actNuclei$y) - sizeNCrossU)
#             crossD = (as.numeric(actNuclei$y) + sizeNCrossD)
#             if ((as.numeric(actNuclei$x) - sizeNCrossL) < 
#                   1) {
#               crossL = 1
#             }
#             if ((as.numeric(actNuclei$y) - sizeNCrossU) < 
#                   1) {
#               crossU = 1
#             }
#             if ((as.numeric(actNuclei$x) + sizeNCrossR) > 
#                   dim(paintedNuclei)[1]) {
#               crossR = dim(paintedNuclei)[1]
#             }
#             if ((as.numeric(actNuclei$y) + sizeNCrossD) > 
#                   dim(paintedNuclei)[2]) {
#               crossD = dim(paintedNuclei)[2]
#             }
#             paintedNuclei[crossL:crossR, as.numeric(actNuclei$y), 
#                           1] = 1
#             paintedNuclei[crossL:crossR, as.numeric(actNuclei$y), 
#                           2] = 0
#             paintedNuclei[crossL:crossR, as.numeric(actNuclei$y), 
#                           3] = 0
#             paintedNuclei[as.numeric(actNuclei$x), crossU:crossD, 
#                           1] = 1
#             paintedNuclei[as.numeric(actNuclei$x), crossU:crossD, 
#                           2] = 0
#             paintedNuclei[as.numeric(actNuclei$x), crossU:crossD, 
#                           3] = 0
#           }
#         }
#         writeImage(paintedNuclei, filenameImage)
#         message("Image saved")
#       }
#       else {
#         message("You have to specify an image filename beforehand in order to save images.")
#       }
#     }
#     if (key == "c") {
#       classCounter <<- classCounter + 1
#       if (classCounter > length(classes)) {
#         classCounter <<- 1
#       }
#       title(sub = actValues, col.sub = "white")
#       actClass <<- classes[classCounter]
#       actValues <<- paste("Class:", actClass, "Action:", 
#                           action)
#       print(paste("New class:", actClass))
#       title(sub = actValues, col.sub = "black")
#       NULL
#     }
#     if (key == "d") {
#       print("Delete points")
#       remove <<- TRUE
#       title(sub = actValues, col.sub = "white")
#       actClass <<- classes[classCounter]
#       action <<- "delete"
#       actValues <<- paste("Class:", actClass, "Action:", 
#                           action)
#       title(sub = actValues, col.sub = "black")
#       NULL
#     }
#     if (key == "h") {
#       if (activateHull == TRUE) {
#         message("Hull deactivated")
#         title(sub = actValues, col.sub = "white")
#         action <<- ""
#         actValues <<- paste("Class:", actClass, "Action:", 
#                             action)
#         activateHull <<- FALSE
#         title(sub = actValues, col.sub = "black")
#       }
#       else {
#         message("Hull activated")
#         title(sub = actValues, col.sub = "white")
#         action <<- "Hull"
#         actValues <<- paste("Class:", actClass, "Action:", 
#                             action)
#         activateHull <<- TRUE
#         title(sub = actValues, col.sub = "black")
#       }
#       NULL
#     }
#     if (key == "a") {
#       print("Add points")
#       remove <<- FALSE
#       title(sub = actValues, col.sub = "white")
#       actClass <<- classes[classCounter]
#       action <<- "add"
#       actValues <<- paste("Class:", actClass, "Action:", 
#                           action)
#       title(sub = actValues, col.sub = "black")
#       NULL
#     }
#     if (key == "Right") {
#       print("block")
#       actBlock <<- actBlock + 1
#       if (actBlock > (nblocks * nblocks)) {
#         actBlock <<- 1
#       }
#       refresh()
#       print(paste("Block number:", actBlock))
#       NULL
#     }
#     if (key == "Left") {
#       actBlock <<- actBlock - 1
#       if (actBlock < 1) {
#         actBlock <<- (nblocks * nblocks)
#       }
#       refresh()
#       print(paste("Block number:", actBlock))
#       NULL
#     }
#   }
#   dragmousedown <- function(buttons, x, y) {
#     startx <- x
#     starty <- y
#     clicked.x <- try(grconvertX(x, from = "ndc", to = "user"))
#     clicked.y <- try(grconvertY(y, from = "ndc", to = "user"))
#     oldX <<- clicked.x
#     oldY <<- clicked.y
#     point = round(c(clicked.x, clicked.y))
#     actPosition = blockPosition[blockPosition$block == actBlock, 
#                                 ]
#     point[1] = actPosition$xs + point[1]
#     point[2] = abs((actPosition$ye - actPosition$ys)) - point[2]
#     point[2] = actPosition$ys + point[2]
#     if (point[1] >= 1 & point[1] < dim(segmentedImage)[1] & 
#           point[2] >= 1 & point[2] < dim(segmentedImage)[2]) {
#       index = segmentedImage[point[1], point[2]]
#       if (!activateHull) {
#         if (remove == FALSE) {
#           print("add point")
#           print(paste("X:", round(point[1]), "Y:", round(point[2]), 
#                       "Class:", actClass, "Nucleus:", index))
#           labeledPoints <<- rbind(labeledPoints, c(index, 
#                                                    point[1], point[2], actClass, round(clicked.x), 
#                                                    round(clicked.y), actBlock))
#         }
#         else {
#           print("remove point")
#           print(paste("X:", round(point[1]), "Y:", round(point[2]), 
#                       "Class:", actClass, "Nucleus:", index))
#           if (index > 0) {
#             nucleiIndexToRemove = which(labeledPoints$index == 
#                                           index)
#             if (length(nucleiIndexToRemove) > 0) {
#               labeledPoints <<- labeledPoints[-nucleiIndexToRemove, 
#                                               ]
#             }
#             if (dim(labeledPoints)[1] == 0) {
#               labeledPoints = data.frame(t(rep(0, 7)), 
#                                          stringsAsFactors = FALSE)
#               colnames(labeledPoints) = c("index", "x", 
#                                           "y", "classCell", "xLocal", "yLocal", 
#                                           "block")
#               labeledPoints <<- labeledPoints
#             }
#           }
#         }
#       }
#     }
#     else {
#       message("Point outside device")
#     }
#     NULL
#   }
#   dragmousemove = function(buttons, x, y) {
#     if (activateHull) {
#       clicked.x <- grconvertX(x, from = "ndc", to = "user")
#       clicked.y <- grconvertY(y, from = "ndc", to = "user")
#       if (!is.null(oldX) & !is.null(oldY)) {
#         if (sqrt((clicked.x - oldX)^2 + (clicked.y - 
#                                            oldY)^2) > 0.5) {
#           "draw"
#           lines(c(oldX, clicked.x), c(oldY, clicked.y), 
#                 col = classColours[which(classes == actClass)])
#           convHull <<- rbind(convHull, c(clicked.x, clicked.y))
#           oldX <<- clicked.x
#           oldY <<- clicked.y
#         }
#       }
#     }
#     NULL
#   }
#   dragmouseup <- function(buttons, x, y) {
#     if (activateHull) {
#       if (!is.null(dim(convHull)[1])) {
#         actPosition = blockPosition[blockPosition$block == 
#                                       actBlock, ]
#         convHull[, 2] = abs((actPosition$ye - actPosition$ys) - 
#                               convHull[, 2])
#         convHull[, 2] = actPosition$ys + convHull[, 2]
#         convHull[, 1] = actPosition$xs + convHull[, 1]
#         convHullS = convHull[sample(1:dim(convHull)[1], 
#                                     dim(convHull)[1], replace = F), ]
#         actPointsXYBlockIndex = which(xyCell[, 1] < actPosition$xe & 
#                                         xyCell[, 1] >= actPosition$xs & xyCell[, 2] < 
#                                         actPosition$ye & xyCell[, 2] >= actPosition$ys)
#         actPointsXYBlock = xyCell[actPointsXYBlockIndex, 
#                                   ]
#         tr = chull(convHullS[, 1], convHullS[, 2])
#         pointsInHull = in.chull(actPointsXYBlock[, 1], 
#                                 actPointsXYBlock[, 2], convHullS[tr, 1], convHullS[tr, 
#                                                                                    2])
#         indexPointsInHull = actPointsXYBlockIndex[which(pointsInHull == 
#                                                           1)]
#         newPoints = cbind(indexPointsInHull, actPointsXYBlock[pointsInHull, 
#                                                               1], actPointsXYBlock[pointsInHull, 2], replicate(length(indexPointsInHull), 
#                                                                                                                actClass), actPointsXYBlock[pointsInHull, 1], 
#                           actPointsXYBlock[pointsInHull, 2], replicate(length(indexPointsInHull), 
#                                                                        actBlock))
#         existingIndices = intersect(labeledPoints$index, 
#                                     indexPointsInHull)
#         if (length(existingIndices) > 0) {
#           message("Replace label")
#           labeledPoints <<- labeledPoints[-existingIndices, 
#                                           ]
#         }
#         colnames(newPoints) = c("index", "x", "y", "classCell", 
#                                 "xLocal", "yLocal", "block")
#         labeledPoints <<- rbind(labeledPoints, newPoints)
#         refresh()
#         clicked.x <- grconvertX(x, from = "ndc", to = "user")
#         clicked.y <- grconvertY(y, from = "ndc", to = "user")
#         oldX <<- NULL
#         oldY <<- NULL
#         convHull <<- c()
#       }
#     }
#     NULL
#   }
#   setGraphicsEventHandlers(prompt = "Click to label cells, hit q to quit", 
#                            onMouseMove = dragmousemove, onMouseUp = dragmouseup, 
#                            onMouseDown = dragmousedown, onKeybd = keydown)
#   getGraphicsEvent()
#   return(labeledPoints)
# }
# # <environment: namespace:CRImage>




# 
# ################
# processingOverview <- function(filename="",imgname="",dispMet="raster",offsetGreen=0.15,offsetRed=0.15,writereport=FALSE,
#                                displayprocessing=FALSE,
#                                areaThresholdMin=5, areaThresholdMax=100,foundImgs="",
#                                channel="",
#                                brushSize=3,
#                                adaptiveWidth=10,adaptiveHeight=10,adaptiveOffset=0.15,
#                                kernSize=3) #,...)
# {
#   rawimg <- readImage(filename)
#   #   if (displayprocessing)    showMe(rawimg)
#   rawimg_red <- rawimg[,,1]
#   rawimg_green <- rawimg[,,2]
#   
#   #   if (displayprocessing)    showMe(EBImage::combine(rgbImage(red=rawimg_red),rgbImage(green=rawimg_green),along=2))
#   flo = makeBrush(brushSize, shape='disc', step=FALSE)^2
#   flo <- flo/sum(flo)
#   
#   thre_red <- thresh(filter2(rawimg_red,flo),w=adaptiveWidth,h=adaptiveHeight,offset=adaptiveOffset)
#   thre_green <- thresh(filter2(rawimg_green,flo),w=adaptiveWidth,h=adaptiveHeight,offset=adaptiveOffset)
#   
#   # if needed with a step of smoothing & co
#   kern <- makeBrush(size=kernSize,shape="disc")
#   
#   # calculating the distance maps to initialize the watershed & performing the watershed transformation
#   dmthre_green <- distmap(thre_green)
#   wsthre_green <- watershed(dmthre_green)
#   dmthre_red <- distmap(thre_red)
#   wsthre_red <- watershed(dmthre_red)
#   
#   #
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
#   shapeAndMomfeats_red <- as.data.frame(computeFeatures(segm_red,rawimg_red,methods.ref=c(),xname="cell"))
#   allAreas_red <- shapeAndMomfeats_red$cell.0.s.area
#   indexes_red <- 1:nrow(shapeAndMomfeats_red)
#   enoughAreas_red <- which((allAreas_red > areaThresholdMin) & (allAreas_red < areaThresholdMax)) # maxbe different between cells on different channels
#   notEnoughAreas_red <- setdiff(indexes_red,enoughAreas_red)
#   notForShape_red <- c() # we need to define what else will be kicked out
#   filterOut_red <- union(notEnoughAreas_red,notForShape_red)
#   
#   # filtering on the green channel
#   shapeAndMomfeats_green <- as.data.frame(computeFeatures(segm_green,rawimg_green,methods.ref=c(),xname="cell"))
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
#   allImgs <- EBImage::combine(rawimg,rgbImage(green=obj_green,red=obj_red),rgbImage(green=cells_green,red=cells_red))
#   segmImgs <- EBImage::combine(rawimg,rgbImage(red=segm_red),rgbImage(green=segm_green))
#   if (displayprocessing)    display(allImgs,method="raster",all=TRUE)
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
#     if (displayprocessing)  display(segmImgs,method="raster",all=TRUE)
#     pdf(file=paste(processingFolder,"/thresholdOverviews/threshold_overview_",imgName,".pdf",sep=""))
#     display(segmImgs,method="raster",all=TRUE)
#     dev.off()
#     if (displayprocessing)  display(allImgs,method="raster",all=TRUE)
#     pdf(file=paste(processingFolder,"/processingOverviews/featureSel_overview_",imgName,".pdf",sep=""))
#     display(allImgs,method="raster",all=TRUE)
#     dev.off()
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
#   if (exists("foundImgs") & exists("imgsJustNames") )
#     cat("Done with image",filename," - ", match(filename,foundImgs) ," of ", length(foundImgs), "\n",sep="")
# }
# 
# 
# ################
# 
# processReportForMOSAIC <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
# {
#   framestats <- read.delim(filename,sep="\t",stringsAsFactors=FALSE,header=TRUE)
#   
#   candidates <- framestats[,1:2]
#   #   candidates$m0 <- rep(0,nrow(candidates))
#   #     candidates$m2 <- rep(0,nrow(candidates))
#   #   names(candidates) <- c("frame",j,"","")
#   names(candidates) <- c("frame",j)
#   
#   
#   cat(paste0("frame ",j-1,"\n"), file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""))
#   write.table(round(candidates,3),
#               file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
#               row.names=FALSE, sep=" ",col.names=F,quote=F,append=T
#   )
#   
#   
#   
# }

# 
# ################
# processReportForMOSAIC_2 <- function(filename,areaThreshold=1,shapeThreshold=0.5,eccenThreshold=0.7,folderPostprocessing="")
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
#   write.table(candidates[,1:2],
#               file=paste(folderPostprocessing,"/processed_",strsplit(filename,split="/")[[1]][length(strsplit(filename,split="/")[[1]])],sep=""),
#               row.names=FALSE, sep=" ",col.names=F
#   )
# }




# 
# 
# ## attempts via tcltk?!
# 
# mywait <- function() {
#   tt <- tktoplevel()
#   tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
#           side='bottom')
#   tkpack( tkbutton(tt, text='Reject', command=function()tkdestroy(tt)),
#           side='bottom')
#   tkbind(tt,'<Key>', function()tkdestroy(tt) )
#   
#   tkwait.window(tt)
# }
# 
# myprompt <- function() {
#   tt <- tktoplevel()
#   tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
#           side='bottom')
#   tkbind(tt,'<Key>', function()tkdestroy(tt) )
#   
#   tkwait.window(tt)
# }   
# 
# 
# 
# 
# tt <- tktoplevel()  # Create a new toplevel window
# 
# tktitle(tt) <- "Simple Dialog"  # Give the window a title
# 
# # Create a variable to keep track of the state of the dialog window:
# #  If the window is active,                                            done = 0
# #  If the window has been closed using the OK button,                  done = 1
# #  If the window has been closed using the Cancel button or destroyed, done = 2
# done <- tclVar(0)   # tclVar() creates a Tcl variable
# 
# # Create two buttons and for each one, set the value of the done variable
# # to an appropriate value
# OK.but <- tkbutton(tt, text = "  Accept  ",
#                    command = function() tclvalue(done) <- 1)
# Cancel.but <- tkbutton(tt, text = "Reject",
#                        command = function() tclvalue(done) <- 2)
# 
# # Place the two buttons on the same row in their assigned window (tt)
# tkgrid(OK.but, Cancel.but)
# 
# # Capture the event "Destroy" (e.g. Alt-F4 in Windows) and when this happens,
# # assign 2 to done
# tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)
# 
# tkfocus(tt)         # Place the focus to our tk window
# 
# # # Do not proceed with the following code until the variable done is non-zero.
# # # (but other processes can still run, i.e., the system is not frozen)
# # tkwait.variable(done)
# # 
# # # The variable done is now non-zero, so we would like to record its value before
# # # destroying the window tt.  If we destroy it first, then done will be set to 2
# # # because of our earlier binding, but we want to determine whether the user
# # # pressed OK (i.e., see whether done is equal to 1)
# 
# doneVal <- as.integer(tclvalue(done))   # Get and coerce content of a Tcl variable
# tkdestroy(tt)
# 
# # Test the result
# if (doneVal == 1) tkmessageBox(message = "You pressed OK!")
# if (doneVal == 2) tkmessageBox(message = "You either pressed Cancel or destroyed the dialog!")
# 
# 
# 
# 
# 
# TCL_evaluateTrajectoryList <- function(trajectorylist,
#                                        rawframelist,
#                                        preprocessedframelist
#                                        #,trajID
# )
# {
#   #   out <- vector("list",length(trajectorylist))
#   out <- trajectorylist # initialized as the input object, which is afterwards modified in the corresponding slot
#   #   id <- 1 # just for trajId =1 now, then could be actually be done cycling on the trajectories
#   for (id in 1:length(trajectorylist))
#   {
#     cat("Evaluating trajectory",id,"...\n")
#     # create painted trajs...
#     paintedTraj <- paintTrajectory(trajectorylist,rawframelist,preprocessedframelist,id)
#     # display it for checking purposes
#     #     fullInspection.FrameList(paintedTraj)
#     # export it also as gif
#     export.FrameList(paintedTraj,nameStub=paste0("evaluation_traj_",id),createGif=TRUE,removeAfterCreatingGif=TRUE)
#     
#     # "interactive" part, asking the user whether the trajectory is correct
#     
#     
#     # best thing, prompt for something like "did you like the trajectory? :)"
#     #   interactive() <- TRUE # does not work-..
#     #   if(interactive()==FALSE)  userInput <- readline(prompt="Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
#     
#     
#     cat("Should I keep this trajectory? --- 0: NO, 1:YES --- no other values allowed")
#     mywait()
#     
#     
#     #     out[[id]]$keep <- as.logical(as.numeric(userInput))
#     
#     
#     ## TODO somehow not expecting the user prompting the value... "no interactive run".. 
#   }
#   
#   return(out)
# }
# 
# 
