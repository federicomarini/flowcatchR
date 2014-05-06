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

