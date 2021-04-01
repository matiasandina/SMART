#' Rudamentary application of histogram equalization of an image on certain channel using Rvision
#' @export
eq_hist <- function(image, channel){
  # create empty zeros vector for lookup table
  lut <- rep(0, 256)
  # calculate histogram
  h <- Rvision::imhist(image)

  minBinNo = 0
  maxBinNo = 255

  # find the first zero on the channel of interest in the histogram
  # caution, which.max is not max(which)
  # this will be the first non-zero count value
  minBinNo = which.min(h[,channel] == 0)
  # we subtract from 256 because we are using which.min on the rev()
  maxBinNo = 256 - which.min(rev(h[,channel]) == 0)
  # we add one to get the correct position
  maxBinNo = maxBinNo + 1

  # replace values in lut
  for (i in 1:length(lut)) {
    if (i < minBinNo) {
      lut[i] <- 0
    } else if (i > maxBinNo) {
      lut[i] <- 255
    }  else {
      lut[i] = round(255.0*(i-minBinNo)/(maxBinNo-minBinNo)+0.5)

    }
  }

  enhanced <- Rvision::LUT(balloon, lut)
  return(enhanced)
}
