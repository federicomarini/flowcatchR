
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
#' @examples
#' \dontrun{
#' oneImg <- subset.FrameList(MesenteriumSubset,framesToKeep = 1)
#' showMe(preprocess(oneImg)[[1]]$image)
#' showMe(reproducibleColorLabels(preprocess(oneImg)[[1]]$image[,,1]))
#' }
#' 
#' @export
#' @author Federico Marini, \email{marinif@@uni-mainz.de}, 2014
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


