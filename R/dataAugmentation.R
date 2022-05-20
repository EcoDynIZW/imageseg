#' Data augmentation: rotating and mirroring images, and adjusting colors
#'
#' @importFrom stats runif
#' @importFrom magick image_rotate
#' @importFrom magick image_flip
#' @importFrom magick image_flop
#'
#' @description Rotate and/or mirror images to create augmented training data. Optionally, apply a random shift in brightness, saturation and hue to a certain percentage of the images
#'
#' @param images  list. Output of \code{loadImages}. List with two items ($info: data frame with information about images, $img: tibble containing magick images)
#' @param subset integer. Indices of images to process. Can be useful for only processing subsets of images (e.g. training images, not test/validation images).
#' @param rotation_angles   integer. Angles in which to rotate images using \code{\link[magick]{image_rotate}})?
#' @param flip  logical. mirror along horizontal axis (turn images upside-down using \code{\link[magick]{image_flip}})?
#' @param flop  mirror along vertical axis (switch left and right) using \code{\link[magick]{image_flop}})?
#' @param brightness_shift_lim  numeric. Lower and upper limits for argument \code{brightness} in \code{\link[magick]{image_modulate}}
#' @param saturation_shift_lim  numeric. Lower and upper limits for argument \code{saturation} in \code{\link[magick]{image_modulate}}
#' @param hue_shift_lim  numeric. Lower and upper limits for argument \code{hue} in \code{\link[magick]{image_modulate}}
#' @param fraction_random_BSH numeric. Fraction of images to apply random brightness / saturation / hue shifts to (between 0 and 1)
#' @details For creating training data for canopy, rotation and mirroring in both axes is appropriate. For understory vegetation density, only flop images (don't flip), and don't apply a hue shift since recognition of the orange flysheet is color-critical.
#'
#' @export
#' @return A list with 2 elements: $info, a data frame with information about the images, and $img, a tibble with magick images
#' @examples
#'
#' # Example 1: Canopy
#' wd_images_can <- system.file("images/canopy/resized",
#'                              package = "imageseg")
#'
#' images_can <- loadImages(imageDir = wd_images_can)
#'
#' images_can_aug <- dataAugmentation(images = images_can,
#'                                    rotation_angles = c(0, 90, 180, 270),
#'                                    flip = TRUE,
#'                                    flop = TRUE)
#' images_can_aug
#'
dataAugmentation <- function(images,
                             subset = NULL,
                             rotation_angles = 0,   # for understory, set to 0 only
                             flip = FALSE,
                             flop = FALSE,
                             brightness_shift_lim = c(90, 110),
                             saturation_shift_lim = c(95, 105),
                             hue_shift_lim = c(80, 120),
                             fraction_random_BSH = 0) {


  if(fraction_random_BSH < 0) stop("fraction_random_BSH must be postive")
  if(fraction_random_BSH > 1) stop("fraction_random_BSH must be between 0 and 1")


  if(any(rotation_angles != 0)){
    images_aug_info <- images$info
    if(unique(images_aug_info$width) != unique(images_aug_info$height)) message("width and height of images differs, and there are rotation angles that are not 0. This may lead to invalid model input. Make sure that rotating is valid for your input")
  }

  if(!0 %in% rotation_angles) warning("0 is not in rotation_angles. That will remove the original images from the augmented image set")

  if(any(rotation_angles %% 90 != 0)) warning("Some rotation_angles are not multiples of 90. These rotated images will have different dimensions than the originals and might not be valid model input. ")
 
  if(!is.null(subset)) {
    images$info <- images$info[subset,]
    images$img <- images$img[subset]
  }
  
  # apply rotation to all images 
  images_aug_list <- lapply(rotation_angles, FUN = function(degrees) magick::image_rotate(images$img, degrees = degrees))
  images_aug_info <- lapply(rotation_angles, FUN = function(x) data.frame(cbind(images$info, rotation = x)))
  images_aug_info <- do.call(rbind, images_aug_info)



  # mirror along horizontal axis
  if(isTRUE(flip)) {
    images_aug_list <- c(images_aug_list, magick::image_flip(images$img))
    images_aug_info <- rbind(cbind(images_aug_info, flip = FALSE),
                                  cbind(images$info, rotation = 0, flip = TRUE))
  } else {
    images_aug_info <- cbind(images_aug_info, flip = FALSE)
  }

  # mirror along vertical axis
  if(isTRUE(flop)){
    images_aug_list <- c(images_aug_list, magick::image_flop(images$img))
    images_aug_info <- rbind(cbind(images_aug_info, flop = FALSE),
                             cbind(images$info, rotation = 0, flip = FALSE, flop = TRUE))
  } else {
    images_aug_info <- cbind(images_aug_info, flop = FALSE)
  }

  # combine into one 'magick tibble' (like input)
  images_aug <- do.call("c", images_aug_list)
  attr(images_aug, "info") <- images_aug_info


  if(fraction_random_BSH != 0) {
    out2 <- randomBSH(img = images_aug,
                      fraction_random_BSH = fraction_random_BSH,
                      brightness_shift_lim = brightness_shift_lim,
                      saturation_shift_lim = saturation_shift_lim,
                      hue_shift_lim = hue_shift_lim)
  } else {
    out2 <- list(info = images_aug_info,
                 img = images_aug)
  }

  return(out2)
}





# modulate brightness, saturation and hue randomly
randomBSH <- function(img,
                      fraction_random_BSH,
                      brightness_shift_lim,
                      saturation_shift_lim,
                      hue_shift_lim)
{


  modify_these <- sort(sample(1:length(img), size = round(fraction_random_BSH * length(img))))
 
  df_info <- attr(img, "info")
  df_info$brightness_shift <- 100
  df_info$saturation_shift <- 100
  df_info$hue_shift        <- 100

    for(i in modify_these){

    brightness_shift <- runif(1,
                              brightness_shift_lim[1],
                              brightness_shift_lim[2])
    saturation_shift <- runif(1,
                              saturation_shift_lim[1],
                              saturation_shift_lim[2])
    hue_shift <- runif(1,
                       hue_shift_lim[1],
                       hue_shift_lim[2])

    img_modified <- magick::image_modulate(img[i],
                                           brightness = brightness_shift,
                                           saturation =  saturation_shift,
                                           hue = hue_shift)
    
    img[i] <- img_modified

    df_info[i,]$brightness_shift <- brightness_shift
    df_info[i,]$saturation_shift <- saturation_shift
    df_info[i,]$hue_shift        <- hue_shift
  }

  return(list(info = df_info,
              img = img))
}
