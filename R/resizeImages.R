
#' Resize and save images
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom magrittr %>%
#' @importFrom foreach `%dopar%` foreach
#' @importFrom magick image_resize
#' @importFrom magick image_modulate
#' @importFrom magick image_convert
#' @importFrom magick image_crop
#' @importFrom magick image_write
#' @importFrom magick image_threshold
#'
#' @param imageDir Character. Directory containing raw images
#' @param fileNames character. File names to load (they will still be filtered by \code{pattern}, if defined)
#' @param pattern character. Pattern to search in file names
#' @param patternInclude logical. Include images with pattern in file names (TRUE) or exclude (FALSE)
#' @param type character. "canopy" or "understory". Will set image dimensions accordingly to predefined c(256, 256) or c(160, 256), respectively (optional). Alternatively, use \code{dimensions}.
#' @param dimensions integer. image dimensions provides as c(width, height) in pixels. If specified, overrides \code{type}
#' @param validRegion character. If defined, use string as argument \code{geometry} in \code{\link[magick]{image_crop}} (output of \code{\link[magick]{geometry_area}}), which will crop all images to the same region before resizing (optional). If undefined, don't crop.
#' @param preserveAspect logical. If TRUE, images will be cropped to aspect ratio of output before resizing (thus preserving original aspect ratio, but losing parts of the image). If FALSE, images will be simply resized from their input size to the desired output (not preserving aspect ratio).
#' @param filter character. Resampling filter. Passed to argument \code{filter} in \code{\link[magick]{image_resize}}. See \code{magick::filter_types()} for available options. Default is LanczosFilter.
#' @param colorspace character. If defined, image will be converted to the requested colorspace. If undefined, colorspace will remain unchanged. Must be a valid argument to  \code{magick::colorspace_types()}. In practice, only "sRGB" and "Gray" will be relevant.
#' @param binary logical. If colorspace is "Gray", make the output binary?
#' @param gravity if preserveAspect = TRUE and images need to be cropped, the \code{gravity} argument to use in \code{\link[magick]{image_crop}}.
#' @param imageFormats character. Image file formats to read.
#' @param outDir character. Directory to save resized images in.
#' @param cores integer. Number of cores to use for parallel processing
#' @param compression character. Compression type to use in \code{\link[magick]{image_write}}. See \code{\link[magick]{compress_types}}. By default, "Lossless" for grayscale images, "Undefined" for color images.
#'
#' @details Resizing is done by \code{\link[magick]{image_resize}} and will ensure that the resized images have exactly the desired dimensions.
#'
#' If \code{preserveAspect = TRUE}, input images will first be cropped to the maximum area with the aspect ratio of the desired output (1:1 (square) for \code{type = "canopy"}, 5:8 for \code{type = "understory"}), by default in the center of the input image (argument \code{gravity}). This will usually lead to the loss of parts of the image, but the remaining part of the image is not deformed compared to the original.
#' Alternatively, if \code{preserveAspect = FALSE}, input images will be resized to the requested dimensions without cropping (thus no loss of part of the image), but the aspect ratio changes. If aspect ratio changes too strongly it may negatively affect model performance.
#'
#' Resizing is done using "!" in the geometry syntax. See \code{\link[magick]{geometry}} for details.
#'
#' compression = "Lossless" is used to ensure no compression artefacts in saved images (which would for example introduce grayscale values in black/white images). If small file sizes are important, you can change it to save compressed images.
#'
#' @return No R output, only resized images are saved on disk
#' @export
#'
#' @examples
#' # Example 1: Canopy
#' wd_can <- system.file("images/canopy/raw",
#'                       package = "imageseg")
#'
#' wd_out_can <- file.path(tempdir(), "canopy", "resized")
#' resizeImages(imageDir = wd_can,
#'              type = "canopy",
#'              outDir = wd_out_can)
#'
#' filename_resized <- list.files(wd_out_can, full.names = TRUE)
#'
#' # check output
#' img_can <- magick::image_read(filename_resized)
#' img_can
#'
#' # Example 2: Understory
#' wd_us <- system.file("images/understory/raw",
#'                       package = "imageseg")
#' wd_out_us <- file.path(tempdir(), "understory", "resized")
#'
#' # note, these are png images
#' resizeImages(imageDir = wd_us,
#'              type = "understory",
#'              outDir = wd_out_us)
#'
#' filename_resized <- list.files(wd_out_us, full.names = TRUE)
#'
#' # check output
#' img_us <- magick::image_read(filename_resized)
#' img_us
#'
resizeImages <- function(imageDir,
                         fileNames,
                         pattern,
                         patternInclude = TRUE,
                         type,
                         dimensions,
                         validRegion,
                         preserveAspect = TRUE,
                         filter = NULL,
                         colorspace,
                         binary,
                         gravity = "Center",
                         imageFormats = c("JPG|TIF|PNG|JPEG|TIFF"),
                         outDir,
                         cores = 1,
                         compression = "Lossless"){



  if(methods::hasArg(type)){
    type <- match.arg(type, c("canopy", "understory"))
    if(type == "canopy") {
      imgWidth  <- 256
      imgHeight <- 256
    }
    if(type == "understory"){
      imgWidth  <- 160
      imgHeight <- 256
    }
  }


  # override default dimensions if requested by user
  if(methods::hasArg(dimensions)) {
    if(methods::hasArg(type)) warning("'dimensions' is defined, therefore 'type' will be ignored")
    if(length(dimensions) != 2) stop("'dimensions' must have length 2 (width, height)")
    imgWidth  <- dimensions[1]
    imgHeight <- dimensions[2]
  }


  if(!methods::hasArg(type) & !methods::hasArg(dimensions)) stop("Image size is undefined. Please set either 'type' or 'dimensions'.")

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

  if(length(lf_images) == 0)    stop("no images found")

  if(hasArg(colorspace)) stopifnot(colorspace %in% magick::colorspace_types())

  if(methods::hasArg(binary)) {
    if(!is.logical(binary)) stop("binary can only be TRUE or FALSE")
  } else {
    binary <- FALSE
  }

  if(hasArg(imageDir))  message(paste("Processing", length(lf_images), "images in", imageDir, " on", cores, "cores."))
  if(!hasArg(imageDir)) message(paste("Processing", length(lf_images), "images on", cores, "cores."))

  if(!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

  # creaste subdirectories, if there are any
  if(length(list.dirs(imageDir)) >= 2) {
    sapply(file.path(outDir, dir(imageDir)), dir.create, showWarnings = FALSE)
  }

  # prevent CRAN check note due to i in foreach below
  i <- NULL

  if(cores > 1){
    # settings for parallel processing (use all available cores minus one)
    numb_cores = parallel::detectCores() - 1
    if(cores > numb_cores) stop("cores exceeds the detected number of cores (minus 1) in your system")
    #if(numb_cores == 0){numb_cores = 1}
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl, cores = cores)

    if(!methods::hasArg(validRegion)) {
      validRegion <- NA
    } else {
      if(hasArg(gravity)) warning("'validRegion' is defined, therefore 'gravity' will be ignored.")
    }

    if(hasArg(colorspace)) {
      if(colorspace == "Gray" & !hasArg(compression))  compression <- "Lossless"
      if(colorspace == "sRGB" & !hasArg(compression))  compression <- NULL
    } else {
      colorspace <- NA
    }

    foreach::foreach(i = lf_images, .packages=c("magick", "imageseg")) %dopar% {

      # load images
      # ::: is necessary within foreach
      # https://stackoverflow.com/questions/36852140/when-does-a-package-need-to-use-for-its-own-objects
      #img <- imageseg:::imageRead(i)
      img <- imageRead(i)



      if(is.character(validRegion)) {    # if user gave geometry_area string, crop
        img <- image_crop(img, validRegion)
      }


      # data format of output:
      imageFormat_out <- "tif"

      # Option 1: force input image to output size, ignore aspect ratio
      if(!preserveAspect){
        img_resize <- magick::image_resize(img, 
                                           geometry = paste0(imgWidth, "x", imgHeight, "!"),
                                           filter = filter)
      }


      # Option 2: crop input image first to preserve aspect ratio
      if(preserveAspect){
        img_info <- magick::image_info(img)
        imgWidth_orig  <- img_info$width
        imgHeight_orig <- img_info$height

        width_ratio   <- imgWidth_orig  / imgWidth
        height_ratio  <- imgHeight_orig / imgHeight

        expansion_fact <- min(c(width_ratio, height_ratio))

        geom_to_crop <- magick::geometry_size_pixels(width = round(imgWidth * expansion_fact), height = round(imgHeight * expansion_fact))

        img_crop <- magick::image_crop(img, geometry = geom_to_crop, gravity = gravity, repage = TRUE)

        img_resize <- magick::image_resize(img_crop, 
                                           geometry = paste0(imgWidth, "x", imgHeight, "!"),
                                           filter = filter)
      }

      if(is.character(colorspace)) {
        img_resize  <- image_convert(img_resize, colorspace = colorspace)
        }

      if(isTRUE(binary)) {
        img_resize <- image_threshold(img_resize, type = "white", threshold = "50%")
        img_resize <- image_threshold(img_resize, type = "black", threshold = "50%")
      }

      magick::image_write(img_resize,
                          # path = file.path(outDir, paste0(gsub(".", "_", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(i)), fixed = TRUE),
                          #                                 ".", imageFormat_out)),
                          path = file.path(dirname(gsub(imageDir, outDir, i, fixed = T)),
                                           paste0(gsub(".", "_", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(i)), fixed = TRUE),
                                                  ".", imageFormat_out)),
                          compression = compression)

      # free up memory to prevent creation of lots of huge temp files by magick
      gc(verbose = FALSE)
    }
    parallel::stopCluster(cl)
  }


  if(cores == 1) {

    # data type of output:
    imageFormat_out <- "tif"

    pb <- txtProgressBar(0, length(lf_images), style = 3)

    for(i in 1:length(lf_images)){

      # load image
      img <- imageRead(lf_images[i])

      if(methods::hasArg(validRegion)) {
        if(is.character(validRegion)) {    # if user gave geometry_area string, crop
          img <- image_crop(img, validRegion)
        }
      }


      # Option 1: force input image to output size, ignore aspect ratio
      if(!preserveAspect){
        img_resize <- magick::image_resize(img, 
                                           geometry = paste0(imgWidth, "x", imgHeight, "!"),
                                           filter = filter)
      }

      # Option 2: crop input image first to preserve aspect ratio
      if(preserveAspect){
        img_info <- magick::image_info(img)
        imgWidth_orig  <- img_info$width
        imgHeight_orig <- img_info$height

        width_ratio   <- imgWidth_orig  / imgWidth
        height_ratio  <- imgHeight_orig / imgHeight

        expansion_fact <- min(c(width_ratio, height_ratio))

        geom_to_crop <- magick::geometry_size_pixels(width = round(imgWidth * expansion_fact), height = round(imgHeight * expansion_fact))

        img_crop <- magick::image_crop(img, geometry = geom_to_crop, gravity = gravity, repage = TRUE)

        img_resize <- magick::image_resize(img_crop, 
                                           geometry = paste0(imgWidth, "x", imgHeight, "!"),
                                           filter = filter)
      }


      if(hasArg(colorspace)) {
        img_resize  <- image_convert(img_resize, colorspace = colorspace)
        if(colorspace == "Gray" & !hasArg(compression))  compression <- "Lossless"
        if(colorspace == "sRGB" & !hasArg(compression))  compression <- NULL
      }

      if(isTRUE(binary)) {
        img_resize <- image_threshold(img_resize, type = "white", threshold = "50%")
        img_resize <- image_threshold(img_resize, type = "black", threshold = "50%")
      }

      test <- try(magick::image_write(img_resize,
                                      # path = file.path(outDir, paste0(gsub(".", "_", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(lf_images[i])), fixed = TRUE),
                                      #                                 ".", imageFormat_out)),
                                      path = file.path(dirname(gsub(imageDir, outDir, lf_images[i], fixed = T)),
                                                       paste0(gsub(".", "_", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(lf_images[i])), fixed = TRUE),
                                                       ".", imageFormat_out)),
                                      compression = compression))
      if("try-error" %in% class(test)) cat(paste(lf_images[i], test, sep = "\n"))

      setTxtProgressBar(pb, i)

      # free up memory to prevent creation of lots of huge temp files by magick
      gc(verbose = FALSE)
    }
    close(pb)
  }

  return(invisible(NULL))
}
