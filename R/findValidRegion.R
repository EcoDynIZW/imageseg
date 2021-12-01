#' Subset image to valid (informative) region
#'
#' @description Load images, find and crop valid (informative) region. This function removes black borders from images, and is suitable for restricting hemispherical (fisheye) images to the actual informative region in the image center.
#'
#' @param image magick image
#' @param fileName file name of image to load
#' @param threshold numeric. Minimum range (max - min) of grayscale values for rows/columns to be included in the output image.
#'
#' @return A list with 3 items:
#'
#' * img: the magick image.
#' * geometry_area: a geometry string that can be used as argument \code{geometry} in \code{\link[magick]{image_crop}}.
#' * geometry_area_df: a data frame containing the information from geometry_area (can be useful for finding consensus are to crop from many images)
#'
#' @details Images are converted to grayscale according to the formula in Li et al. (2020). L = 0.30R + 0.59G + 0.11B
#' We use a default threshold of 10, but it can be adjusted freely.
#'
#' This function can optionally be called inside \code{\link{resizeImages}} to crop each image to the informative region before resizing to the dimensions expected by the models. It is not recommended though since it may return different crop masks for images and their masks.
#'
#' @section Warning:
#' Depending on the quality of the photographic equipment used, supposedly dark regions of images may be affected by stray light and light diffraction. This will be especially prevalend when using fisheye adapters, e.g. for smartphones. In such cases, the function will not provide reliable output. Adjusting `threshold` may help to a degree, but needs to be set on a case-to-case basis for individual images. In such cases it might be easier to instead use e.g. GIMP to measure out the valid image region.
#'
#' @importFrom magick image_separate
#' @importFrom magick image_crop
#' @importFrom magick image_raster
#' @importFrom magick geometry_area
#' @importFrom grDevices col2rgb
#' @export
#'
#' @references
#' Li, Kexin, et al. "A New Method for Forest Canopy Hemispherical Photography Segmentation Based on Deep Learning." Forests 11.12 (2020): 1366.
#'
#' @examples
#'
#' wd_images_can <- system.file("images/canopy/raw",
#'                               package = "imageseg")
#' lf <- list.files(wd_images_can, full.names = TRUE)
#' img <- findValidRegion(fileName = lf[1])
#' img
#'
#' # finding consensus among multiple images
#' \dontrun{
#' wd_with_many_images <- "..."
#' lf <- list.files(wd_with_many_images)
#' test <- lapply(lf, findValidRegion)
#' # combine geometry_area_df from many images
#' geometry_areas_df <- do.call(rbind, lapply(test, FUN = function(x) x$geometry_area_df))
#' # summary to decide on suitable values
#' summary(geometry_areas_df)
#' }


findValidRegion <- function(image,
                          fileName,
                          threshold = 0.1)  {

  if(methods::hasArg(fileName)){
    if(length(fileName) > 1) warning("fileName has length > 1. Will only process the first image:", fileName[1])
    img <- suppressMessages(loadImages(fileNames = fileName[1]))
    # separate channels
    img_channels <- image_separate(img$img)

  }
  if(methods::hasArg(image)){
    stopifnot(length(image) == 1)
    img <- image
    img_channels <- image_separate(img)
  }

  tmp <- image_raster(img_channels)

  tmp2 <- cbind(tmp, t(col2rgb(tmp$col)))

  # convert image to grayscale using formula from Li et al 2020
  tmp2$L <- 0.30 * tmp2$red + 0.59 * tmp2$green + 0.11 * tmp2$blue


  # # get ranges of "lightness" ($L) values for each row / column
  # # approach is too sensitive to hotpixels
  range_l_x <- tapply(tmp2$L, INDEX = tmp2$x, FUN = function(x) diff(range(x)))
  range_l_y <- tapply(tmp2$L, INDEX = tmp2$y, FUN = function(x) diff(range(x)))

  # range_l_x <- tapply(tmp2$L, INDEX = tmp2$x, FUN = function(x) sd(x))
  # range_l_y <- tapply(tmp2$L, INDEX = tmp2$y, FUN = function(x) sd(x))
  #

  # rows and columns to keep
  x_range_to_keep <- as.numeric(names(range_l_x) [range_l_x >= threshold])
  y_range_to_keep <- as.numeric(names(range_l_y) [range_l_y >= threshold])

  # set geometry to keep
  df_geometry_area <- data.frame(width = diff(range(x_range_to_keep)),
                                 height = diff(range(y_range_to_keep)),
                                 x_off = min(x_range_to_keep) - 1,
                                 y_off = min(y_range_to_keep) - 1)

  geometry_area_tmp <- geometry_area(width = df_geometry_area$width,
                                     height = df_geometry_area$height,
                                     x_off = df_geometry_area$x_off,
                                     y_off = df_geometry_area$y_off)

  # crop image
  img_out <- image_crop(img$img[1], geometry_area_tmp)

  list(img = img_out,
       geometry_area = geometry_area_tmp,
       geometry_area_df = df_geometry_area)

}
