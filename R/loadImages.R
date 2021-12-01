
#' Load image files with magick
#'
#' @importFrom magick image_info
#'
#' @description This function loads images from disk to R, where one can inspect them and then pass them on to \code{\link{imagesToKerasInput}}, which converts them to input for keras (TensorFlow) models.
#' @param imageDir character. Directory containing the images to load
#' @param fileNames character. File names to load (they will still be filtered by \code{pattern}, if defined)
#' @param pattern character. Pattern to search in file names
#' @param patternInclude logical. Include images with pattern in file names (TRUE) or exclude (FALSE)
#' @param imageFormats character. Image file formats to read.
#'
#' @return A list with 2 slots: "img" contains images as a tibble, "info" contains basic information about the images.
#' @export
#'
#' @examples
#' # Example 1: Canopy
#' wd_images_can <- system.file("images/canopy/resized",
#'                              package = "imageseg")
#'
#' images_can <- loadImages(imageDir = wd_images_can)
#' images_can
#'
#' # Example 2: Understory
#' wd_images_us <- system.file("images/understory/resized",
#'                              package = "imageseg")
#' images_us <- loadImages(imageDir = wd_images_us)
#' images_us
#'
loadImages <- function(imageDir,
                              fileNames,
                              pattern,
                              patternInclude = TRUE,
                              imageFormats = c("JPG|TIF|PNG|JPEG|TIFF")) {

  if(methods::hasArg(imageDir)) {
    if(length(imageDir) >= 2) stop("imageDir can only have length 1")

    # list files
    lf_images <- list.files(imageDir, full.names = TRUE, pattern = imageFormats, ignore.case = TRUE, recursive = TRUE)
    if(length(lf_images) == 0) stop(paste0("The directory ", imageDir, " contains no TIFF, JPEG or PNG images. Please make sure the path is correct."))
  }


  if(methods::hasArg(fileNames)) {
    lf_images <- fileNames
  }

  if(methods::hasArg(pattern)) {
    if(!methods::hasArg(patternInclude)) stop(paste("patternInclude must be defined if pattern is defined"))

    stopifnot(is.logical(patternInclude))

    if(isTRUE(patternInclude))   lf_images <- lf_images[grep(pattern, lf_images)]
    if(isFALSE(patternInclude))  lf_images <- lf_images[grep(pattern, lf_images, invert = TRUE)]

  }

  # load images
  img <- imageRead(lf_images)

  message(paste("found", length(img), "images"))

  fileinfo <- cbind(filename = lf_images, magick::image_info(img))

  return(list(info = fileinfo,
              img = img))
}
