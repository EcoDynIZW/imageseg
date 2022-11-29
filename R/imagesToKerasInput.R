
#' Convert magick images in tibble to array for keras
#'
#' @description This function converts a tibble of images into input for TensorFlow models in keras. Specifically, images are converted to 4D arrays (image, height, width, channels). It can process color images and masks (for model training).
#' @param images list. Output of \code{loadImages} or \code{dataAugmentation}. List with two items ($info: data frame with information about images, $img: tibble containing magick images)
#' @param subset integer. Indices of images to process. Can be useful for only processing subsets of images (e.g. training images, not test/validation images).
#' @param type character. Can be "image" or "mask" and will set color channels of array accordingly (optional).
#' @param grayscale logical. Defines color channels of images: 1 if code{TRUE}, 3 if \code{FALSE}.
#' @param n_class For mask images, how many classes do they contain? (note that binary classifications like the canopy model have one class only)
#' @param max integer. Maximum value of output color values range. Can be 1 or 255.
#'
#' @details The function will try to infer the colorspace from images, but if the colorspaces are inconsistent one has to define 'colorspace'.
#' \code{type = "image"} can have either colorspace "sRGB" or "Gray", masks are always "Gray". color images have three color channels in the arrays, grayscale images have one color channel.
#' \code{n_class} is only relevant for masks. It determines the dimensions of the output. The default 1 is the (binary case). Higher values are for multi-class cases. If n_class is 2 or larger, keras::to_categorical() will be applied, and the \code{\link{u_net}} model will use softmax instead of sigmoid activation in the final layer.
#' 
#' By default, color value range will be 0-1. Alternatively, set \code{max} to 255 to create color value range 0-255 (e.g. to create input for Habitat-Net models).
#'
#' @return An array with the following dimensions: image, height, width, channels
#' @export
#'
#' @importFrom keras to_categorical
#' @importFrom magick image_convert
#'
#' @examples
#' # Example 1: Canopy
#' 
#' # images
#' wd_images_can <- system.file("images/canopy/resized",
#'                              package = "imageseg")
#' images_can <- loadImages(imageDir = wd_images_can)
#' x <- imagesToKerasInput(images_can)
#' str(x)   # a 4D array with an attribute data frame
#'
#' # masks
#' 
#' wd_mask_can <- system.file("images/canopy/masks",
#'                              package = "imageseg")
#' masks_can <- loadImages(imageDir = wd_mask_can)
#' y <- imagesToKerasInput(masks_can, type = "mask", grayscale = TRUE)
#' str(y)   # a 4D array with an attribute data frame
#'
#' # Example 2: Understory
#' wd_images_us <- system.file("images/understory/resized",
#'                              package = "imageseg")
#' images_us <- loadImages(imageDir = wd_images_us)
#' x <- imagesToKerasInput(images_us)
#' str(x)   # a 4D array, with an attribute data frame
#'
imagesToKerasInput <- function(images,
                               subset = NULL,
                               type = NULL,
                               grayscale = NULL,
                               n_class = 1,
                               max = 1) {


  if(!is.null(subset)) {
    if(!is.null(images$info)) images$info <- images$info[subset,]
    images$img <- images$img[subset]
  }
  
  # checks
  image_info_df <- magick::image_info(images$img)

  if(length(unique(image_info_df$width)) >= 2)      stop(paste("Multiple values for image width:", paste(unique(image_info_df$width), collapse = ", ")))
  if(length(unique(image_info_df$height)) >= 2)     stop(paste("Multiple values for image height:", paste(unique(image_info_df$height), collapse = ", ")))


  # set colorspace to Gray if type = mask
  if(methods::hasArg(type)){
    type <- match.arg(type, c("image", "mask"))

    if(type == "mask") {
      if(hasArg(grayscale)) if(grayscale != TRUE) message("type = 'mask', therefore grayscale is set to TRUE")
      colorspace <- "Gray"
      channels <- 1
    }
  }


  # guess colorspace if not specified
  if(!methods::hasArg(grayscale)) {
    if(all(image_info_df$colorspace == "Gray")) {
      message("colorspace is Gray")
      channels <- 1
      colorspace <- "Gray"
    }
    if(all(image_info_df$colorspace == "sRGB")) {
      message("colorspace is sRGB")
      channels <- 3
      colorspace <- "sRGB"
    }

    if(length(unique(image_info_df$colorspace)) >= 2 ) stop(paste("Different colorspaces in input images:", paste(unique(image_info_df$colorspace), collapse = ", "), ". Please define 'grayscale'."))
  } else {

    channels <- if (grayscale) 1 else 3
    
    if(channels == 1) colorspace <- "Gray"
    if(channels == 3) colorspace <- "sRGB"
    # colorspace <- match.arg(colorspace, c("sRGB", "Gray"))
    # if(colorspace == "Gray") channels <- 1
    # if(colorspace == "sRGB") channels <- 3

    if(length(unique(image_info_df$colorspace)) >= 2) message(paste("Different colorspaces in input images:", paste(unique(image_info_df$colorspace), collapse = ", "), ". Will convert all to",  colorspace))

    # force colorspace
    images$img <- magick::image_convert(images$img, colorspace = colorspace)
  }




  if(!is.null(images$info)) {
    cols_to_compare <- c("width", "height", "colorspace")
    for(cols_to_compare_tmp in cols_to_compare){
      if(!isTRUE(all.equal(as.data.frame(image_info_df)[, cols_to_compare_tmp], images$info [, cols_to_compare_tmp]))) warning(paste("mismatch between $info slot in images and output of image_info() - column:", cols_to_compare_tmp))
    }
  }


  # Image workflow

  # convert RGB images to matrices, and store results in arrays
  if(!max %in% c(1, 255)) stop("'max' must be 1 or 255")
  scaling_value <- ifelse(max == 1, 255, 1)
  
  if(channels == 3) {
    images_proc <- lapply(images$img,  FUN = function(x) as.integer(x[[1]]) / scaling_value)   # Hex values to integer, integer to float (0...1)
  }

  if(channels == 1) {
    if(n_class == 1){    # single output class
      images_proc <- lapply(images$img,  FUN = function(x) as.integer(x[[1]]) / scaling_value)   # Hex values to integer, integer to float (0...1)

    } else {  # multiple output classes
      images_proc <- lapply(images$img,  FUN = function(x) as.integer(x[[1]]))        # Hex values to integer,
    }
  }

  # create arrays of all images
  array_out <- array(NA, dim = c(length(images_proc),
                                 unique(image_info_df$height),
                                 unique(image_info_df$width),
                                 channels))

  for(i in 0:length(images_proc)){
    array_out[i,,,] <- images_proc[[i]]
  }



  # ensure masks are 0 or 1 only
  if(methods::hasArg(type)) {
    if(type == "mask"){
      if(n_class == 1){
        if(!all(unique(as.vector(array_out)) %in% c(0,1))) {
          message(paste("masks are not discrete. Found", length(unique(as.vector(array_out))), "unique values. Fixed through rounding."))
          array_out <- round(array_out)
        }
      }


      if(n_class >= 2){   # categorical
        if(length(unique(as.vector(array_out))) != n_class) stop(paste0("number of unique values in masks (", length(unique(as.vector(array_out))),
                                                                        ") is not equal to n_class (", n_class, ")"))
        array_out <- to_categorical(array_out, n_class)
      }
    }
  } else {
    if(channels == 1){
      if(!all(unique(as.vector(array_out)) %in% c(0,1))) {
        message("Images are not discrete, but 'type' is undefined. Images will be returned without binarization.")
      }
    }
  }

  message(paste(dim(array_out)[1], "images,", dim(array_out)[3], "x", dim(array_out)[2], "pixels,", dim(array_out)[4], "color channels"))

  if(!is.null(images$info)) {
    attr(array_out, "info") <- images$info
  }

  return(array_out)
}
