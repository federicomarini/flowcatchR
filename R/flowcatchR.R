#' flowcatchR: analyzing time-lapse microscopy imaging, from detection to tracking
#'
#' A toolset to analyze in vivo microscopy imaging data focused on tracking
#' flowing blood cells.
#' 
#' flowcatchR is a set of tools to analyze in vivo microscopy imaging
#' data, focused on tracking flowing blood cells. It guides the steps from
#' segmentation to calculation of features, filtering out particles not of
#' interest, providing also a set of utilities to help checking the quality of
#' the performed operations (e.g. how good the segmentation was). The main
#' novel contribution investigates the issue of tracking flowing cells such as
#' in blood vessels, to categorize the particles in flowing, rolling and
#' adherent. This classification is applied in the study of phenomena such as
#' hemostasis and study of thrombosis development.
#' 
#' @author
#' Federico Marini \email{federico.marini@@uni-mainz.de},
#' Johanna Mazur \email{mazur@@uni-mainz.de},
#' Harald Binder \email{binderh@@uni-mainz.de}
#'
#' Maintainer: Federico Marini \email{federico.marini@@uni-mainz.de}
#' @name flowcatchR
#' @docType package
NULL


#' fullFrameList
#' 
#' A sample FrameList object provided
#' 
#' The sample FrameList object is constituted by a subset of a time-lapse intravital microscopy imaging dataset.
#' Green channel marks leukocytes, red channel focuses on blood platelets.
#' Images are kindly provided by Sven Jaeckel (\email{Sven.Jaeckel@@unimedizin-mainz.de}).
#' 
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
#' @name fullFrameList
#' @docType data
NULL

#' MesenteriumSubset
#' 
#' A sample FrameList object provided
#' 
#' The sample FrameList object is constituted by a subset of a time-lapse intravital microscopy imaging dataset.
#' Green channel marks leukocytes, red channel focuses on blood platelets. 20 frames are provided in this subset.
#' Images are kindly provided by Sven Jaeckel (\email{Sven.Jaeckel@@unimedizin-mainz.de}).
#' 
#' @author Federico Marini, \email{federico.marini@@uni-mainz.de}, 2014
#' @name MesenteriumSubset
#' @docType data
NULL


.FLOWCATCHR_VERSION <- '0.2'
#'
#'
.onAttach <- function(lib, pkg, ...) {
packageStartupMessage(sprintf("\nThis is flowcatchR version %s - A toolset to analyze in vivo microscopy imaging data
for tracking flowing blood cells. Copyright (C) 2014 Federico Marini\n
Type '?flowcatchR' for help or see www.imbei.de for more details", .FLOWCATCHR_VERSION))
}



