#' Model predictions from images based on TensorFlow model
#'
#' @description This function uses a pre-trained TensorFlow model to create predictions from input data. It was mainly designed to predict canopy cover and understory vegetation density from forest habitat photographs using the pre-trained models we provide.
#' @importFrom magrittr %>%
#' @import tibble
#' @import purrr
#' @importFrom grDevices as.raster
#' @importFrom methods hasArg
#' @importFrom stats predict
#' @importFrom keras layer_input
#' @importFrom keras layer_max_pooling_2d
#' @importFrom keras layer_dropout
#' @importFrom keras layer_conv_2d_transpose
#' @importFrom keras layer_concatenate
#' @importFrom keras layer_batch_normalization
#' @importFrom keras layer_activation
#' @importFrom keras layer_upsampling_2d
#'
#' @param model trained model to use in predictions
#' @param x array of images as model input (can be created with \code{\link{imagesToKerasInput}})
#' @param dirOutput character. Directory to save output images to (optional)
#' @param dirExamples character. Directory to save example classification to (optional)
#' @param subsetArea If "circle", vegetation density will be calculated for a circular area in the center of the predicted images. Can also be a number between 0 and 1 (to scale the circle relative to the image dimensions), or a matrix of 0 and 1 in the same dimensions as images in x.
#' @param threshold numeric value at which to split binary predictions. Can be useful to only return high-confidence pixels in predictions. It is not relevant for multi-class predictions.
#' @param returnInput logical. If \code{dirOutput} is defined, save input images alongside output?
#'
#' @details By default, vegetation density will be calculated across the entire input images. If canopy images are hemispherical and have black areas in the corner that should be ignored, set \code{subsetArea} to "circle". If the relevant section of the images is smaller than the image frame, give a number between 0 and 1 (indicating how big the circle is, relative to the image dimensions).
#' Alternatively, provide a custom matrix of 0 and 1 in the same dimensions as the input images in x. 0 indicates areas to ignore in the vegetation calculations, 1 is included. \code{subsetArea = "circle"} only works if input images in x are square.
#'
#' The canopy density models predicts sky and the understory vegetation density model predicts the red flysheet The percentage of these is equivalent to openness (canopy openness or understory openness). This value is in the column "predicted".
#'
#' #' The interpretation of openness depends on context:
#'
#' * Canopy Cover images: openness = Gap Fraction and Fraction Soil
#'
#' * Hemispherical canopy images: openness = Canopy openness and site openness (in flat terrain)
#'
#' See e.g. Gonsamo et al. (2013) for more details.
#'
#' Generally speaking, "predicted" is the percentage of the image that is 1 in the binary prediction.
#'
#'
#' The column "not_predicted" is the opposite (1-predicted). It is thus equivalent to vegetation density in the two vegetation models.
#'
#' Depending on the context, "not_predicted" can for example mean: canopy cover, canopy closure, understory vegetation density.
#' In canopy cover images, the vegetation density corresponds to canopy cover. In hemispherical images, vegetation density corresponds to canopy closure.
#'
#'
#'
#'
#' @return A list. The type and number of list items depends on the classification. For binary classifications (1 prediction class), the following list items are returned:
#'
#' * image (input images)
#' * prediction (model prediction)
#' * prediction_binary (binary prediction, only 0 or 1)
#' * examples (images with their image segmentation results)
#' * summary (data frame with fraction of image predicted)
#' * mask (an image showing the area for which summary statistics were calculated (in white, only if \code{subsetArea} is defined)
#'
#' in multi-class models:
#'
#' * image (input images)
#' * prediction_most_likely (the class with the highest probability, coded in grayscale)
#' * class1 - classX: for each class, the predicted probabilities
#' * examples (images with their image segmentation results)
#' * summary (data frame with fraction of image covered by vegetation (black)).
#' * mask (an image showing the area for which summary statistics were calculated (in white, only if \code{subsetArea} is defined)
#
#'
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#'
#' \dontrun{
#'
#' # Example 1: Canopy
#' wd_images <- system.file("images/canopy/resized",
#'                          package = "imageseg")
#' images <- loadImages(imageDir = wd_images)
#' x <- imagesToKerasInput(images)
#'
#' wd_model_can <- "C:/Path/To/Model"      # change this
#' filename_model_can <- "imageseg_canopy_model.hdf5"
#' model_can <- loadModel(file.path(wd_model_can, filename_model_can))
#'
#'
#' results_can <- imageSegmentation(model = model_can,
#'                                  x = x)
#' results_can$image
#' results_can$prediction
#' results_can$prediction_binary
#' results_can$vegetation
#'
#'
#' # Example 2: Understory
#' wd_images_us <- system.file("images/understory/resized",
#'                              package = "imageseg")
#' images_us <- loadImages(imageDir = wd_images_us)
#' x <- imagesToKerasInput(images_us)
#'
#' # note, here we just specify the complete path, not separate by directory and file name as above
#' model_file_us <- "C:/Path/To/Model/imageseg_understory_model.hdf5"
#' model_us <- loadModel(model_file_us)
#'
#' results_us <- imageSegmentation(model = model_us,
#'                                 x = x)
#' results_us$image
#' results_us$prediction
#' results_us$prediction_binary
#' results_us$vegetation
#'
#' }
#'

imageSegmentation <- function(model,
                              x,
                              dirOutput,
                              dirExamples,
                              subsetArea,
                              threshold = 0.5,
                              returnInput = FALSE)
{


  # TO DO: SAVE EXAMPLES IN FUNCTION OUTPUT BY DEFAULT

  if(!is.null(attr(x, "info"))) {
    info_df <- attr(x, "info")
  }

  if(dim(x)[1] == 1) stop("Please provide 2 or more images in x")

  if(hasArg(subsetArea)) {
    if(inherits(subsetArea, "character")) {
      if(subsetArea != "circle") stop("subsetArea is character, but not 'circle'")
      if(dim(x)[2] != dim(x)[3]) stop("subsetArea can only be 'circle' if the images in x are square")

      subsetArea <- circular.weight(dim(x)[2])
    }  else {
      if(inherits(subsetArea, "matrix")){
        tmp <- table(subsetArea)
        if(length(names(tmp)) > 2) stop("subsetArea is a matrix but has more values than 0 and 1")
        if(!all(c("0", "1") %in% names(tmp))) stop("subsetArea does not contain 0 and 1")
        if(paste(dim(subsetArea), collapse = ",") != paste(dim(x)[2:3], collapse = ",")) stop(paste0("subsetArea has different dimensions than x (",
                                                                                                     paste(dim(subsetArea), collapse = ","), " vs ",
                                                                                                     paste(dim(x)[2:3], collapse = ","), ")"))
      } else {

        if(is.numeric(subsetArea)){
          # if numeric, must be between 0 and 1.
          # number reflects the ratio of circle diameter and image size
          subsetArea <- circular.weight(dim(x)[2], ratio = subsetArea)

        } else {
          stop("subsetArea can only be 'circle', or a matrix of 1 and 0 in the dimensions of the input images (x), or a number between 0 and 1")
        }
      }
    }
  }

  predictions <- model %>% predict(x)

  # how many classes predicted in model output?
  n_class <- dim(predictions)[4]

  # how many pixels
  n_cells <- prod(dim(x)[c(2,3)])


  # this is only to avoid message: no visible binding for global variable '.' which is used below in pmap(), map()
  . <- NULL



  # convert input and prediction output from arrays to RGB images (in a list)
  if(n_class == 1) {
    # if only 1 output class, round all predictions to make binary
    if(threshold == 0.5){
      predictions_binary <- round(predictions)
    } else {
      predictions_binary <- (predictions >= threshold)*1
    }


    if(dim(x)[4] >= 2)        margin_x <- c(1)       # if color image as input
    if(dim(x)[4] == 1)        margin_x <- c(1,4)     # if grayscale image as input

      images_from_prediction <- tibble(
        image = x %>% array_branch(margin_x),
        prediction = predictions[,,,1] %>% array_branch(1),
        prediction_binary = predictions_binary[,,,1] %>% array_branch(1)
      ) %>%
        map_depth(2, function(x) {
          as.raster(x) %>% magick::image_read()
        }) %>%
        map(~do.call(c, .x))
    }



  # if multiple classes, create array with most probable prediction class for each image
  if(n_class != 1) {
    predictions_list <- predictions %>% array_tree(c(1))

    prediction_most_likely <- lapply(predictions_list, apply, c(1,2), which.max)


    # images and most probable class
    images_from_prediction1 <- tibble(
      image = x %>% array_branch(1),
      prediction_most_likely = prediction_most_likely %>% array_branch(1) %>% map(~./n_class)
    ) %>%
      map_depth(2, function(x) {
        as.raster(x) %>% magick::image_read()
      }) %>%
      map(~do.call(c, .x))

    # probabilities of each class
    images_from_prediction2 <-
      predictions %>% array_tree(c(1, 4))  %>%
      map_depth(2, function(x) {
        as.raster(x) %>% magick::image_read()
      }) %>%
      pmap(., c)

    names(images_from_prediction2) <- paste0("class", seq(1, n_class))

    images_from_prediction <- c(images_from_prediction1, images_from_prediction2)
  }



  # if it's a binary classification (1 class only), calculate percentage of predicted areas

  if(n_class == 1){

    # remove subsetArea from values to summarize
    predictions_binary_list <- predictions_binary[,,,1] %>% array_branch(1)

    if(hasArg(subsetArea)) {
      for(i in 1:length(predictions_binary_list)) {
        predictions_binary_list[[i]] [subsetArea == 0] <- NA
      }
    }

    # calculate percentage of prediction / not prediction (prediction = sky, not prediction = canopy cover / vegetation density)
    mean_predicted  <- round(sapply(predictions_binary_list, FUN = function(x) mean(x, na.rm = T)), 3)
    mean_not_predicted <- round(sapply(predictions_binary_list, FUN = function(x) 1 - mean(x, na.rm = T)), 3)

    if(exists("info_df")){
      if(nrow(info_df) != length(mean_not_predicted)) stop("mismatch in length of file info attributes in x and predictions based on x")
      if(nrow(info_df) != length(images_from_prediction$image)) stop("mismatch in length of file info attributes in x and input images in x")
    }
  }  # end     if(n_class == 1)




  # if it's a multi-class prediction, calculate percentage of each predicted class
  if(n_class > 1){

    # remove subsetArea from values to summarize

    if(hasArg(subsetArea)) {
      for(i in 1:length(prediction_most_likely)) {
        prediction_most_likely[[i]] [subsetArea == 0] <- NA
      }
    }


    # count how often each class is predicted to me most likely

    prediction_percentages  <- sapply(prediction_most_likely, table)

    # ifelse only to deal with inconsistent output of table (depending on whether all classes present or not)

    if(inherits(prediction_percentages, "matrix"))   {
      prediction_percentages3 <- data.frame(t(prediction_percentages)) / n_cells
    }
    if(inherits(prediction_percentages, "list"))   {
      prediction_percentages2 <- sapply(prediction_percentages, FUN = function(x) data.frame(rbind(x)))
      prediction_percentages3 <- data.frame(dplyr::bind_rows(prediction_percentages2),
                                            row.names = 1:length(prediction_most_likely)) / n_cells
    }


    colnames(prediction_percentages3) <- paste0("class", 1:ncol(prediction_percentages3))

    # round and replace NA with 0
    prediction_percentages3 <- round(prediction_percentages3, 3)
    prediction_percentages3[is.na(prediction_percentages3)] <- 0


    if(exists("info_df")){
      if(nrow(info_df) != nrow(prediction_percentages3)) stop("mismatch in length of file info attributes in x and predictions based on x")
      if(nrow(info_df) != length(images_from_prediction$image)) stop("mismatch in length of file info attributes in x and input images in x")
    }
  }  # end     if(n_class == 1)




  # if subsetArea is defined, indicate it in output
  if(hasArg(subsetArea)) {
    subsetArea_img <- as.raster(subsetArea) %>% magick::image_read()
    subsetArea_img2 <- magick::image_transparent(magick::image_negate(subsetArea_img), color = "white")

    images_from_prediction <- lapply(images_from_prediction, FUN = magick::image_composite, image = subsetArea_img2, operator = "atop")

    images_from_prediction <- lapply(images_from_prediction, FUN = magick::image_background, color = "white")

    images_from_prediction$mask <- subsetArea_img
  }



  if(hasArg(dirExamples)){
    if(!dir.exists(dirExamples)) dir.create(dirExamples, recursive = TRUE)
    saveExamples <- TRUE
    }

  n_samples_per_image <- 8

  out_list <- list()

  if(n_class == 1) {

    for(i in 1:ceiling(nrow(x) / n_samples_per_image)){
      in_this_sample <- seq(1:n_samples_per_image) + (n_samples_per_image * (i - 1))
      in_this_sample <- in_this_sample[in_this_sample <= length(images_from_prediction$image)]

      #text_to_add <- as.character(mean_not_predicted[in_this_sample])

      out <- magick::image_append(c(
        # plot input image
        magick::image_append(images_from_prediction$image[in_this_sample], stack = TRUE),


        # plot prediction with % canopy cover
        magick::image_append(images_from_prediction$prediction[in_this_sample], stack = TRUE),

        # plot binary prediction
        magick::image_append(images_from_prediction$prediction_binary[in_this_sample], stack = TRUE)
      ))

      out_list[[i]] <- out

      if(hasArg(dirExamples)){
        if(saveExamples){
          magick::image_write(out, path = file.path(dirExamples, paste0("classification_example", i, ".png")))

        }
      }
    }
  }   # end if(n_class == 1)


  if(n_class > 1) {

    for(i in 1:ceiling(nrow(x) / n_samples_per_image)){
      in_this_sample <- seq(1:n_samples_per_image) + (n_samples_per_image * (i - 1))
      in_this_sample <- in_this_sample[in_this_sample <= length(images_from_prediction$image)]

      out <- magick::image_append(c(
        # plot input image
        magick::image_append(images_from_prediction$image[in_this_sample], stack = TRUE),

        # plot most likely class
        magick::image_append(images_from_prediction$prediction_most_likely[in_this_sample], stack = TRUE),

        # plot probability of each class
        map(images_from_prediction[startsWith(names(images_from_prediction), "class")], ~.x[in_this_sample]) %>% map(., magick::image_append, stack = TRUE)

      ))

      out_list[[i]] <- out

      if(hasArg(dirExamples)){
        if(saveExamples){
          magick::image_write(out, path = file.path(dirExamples, paste0("classification_example", i, ".png")))

        }
      }
    }
  }  # end if(n_class > 1)


  images_from_prediction$examples <- Reduce(c, out_list)

  

  if(hasArg(dirOutput)) {

    if(!dir.exists(dirOutput)) dir.create(dirOutput, recursive = TRUE)

    
    if(n_class == 1) output <- "prediction_binary"
    if(n_class > 1)  output <- "prediction_most_likely"
    
    # use original file names if available
    if(exists("info_df")){
      filenames_orig <- strsplit(info_df$filename, .Platform$file.sep, fixed = TRUE)
      filenames_orig <- sapply(filenames_orig, FUN = function(x) x[length(x)])
      

      # remove extension
      filenames_out <- sapply(strsplit(filenames_orig, split = ".", fixed = TRUE), FUN = function(x) x[1])
      
      # check for duplicates
      if(anyDuplicated(filenames_out) != 0){
        warning("Output file names are not unique. Attempting to make them unique by adding data augmentation information.")
        
        
        if("rotation" %in% colnames(info_df)) filenames_out <- paste0(filenames_out, "_rot", info_df$rotation)
        if("flip" %in% colnames(info_df)) filenames_out <- paste0(filenames_out, ifelse(info_df$flip, "_flip", ""))
        if("flop" %in% colnames(info_df)) filenames_out <- paste0(filenames_out, ifelse(info_df$flop, "_flop", ""))
        # if("brightness_shift" %in% colnames(info_df)) filenames_out <- paste0(filenames_out, ifelse(info_df$brightness_shift != 100, "_B", ""))
        # if("saturation_shift" %in% colnames(info_df)) filenames_out <- paste0(filenames_out, ifelse(info_df$brightness_shift != 100, "_S", ""))
        # if("hue_shift" %in% colnames(info_df)) filenames_out <- paste0(filenames_out, ifelse(info_df$brightness_shift != 100, "_H", ""))
        
        
        if(anyDuplicated(filenames_out) != 0) stop("Failed to make file names unique. Please check attr(x, 'info') and ensure there are no duplicates. First duplicate was:\n",
                                                   filenames_orig[anyDuplicated(filenames_out)])
      }
      # filename of classified images
      filenames_out_class <- paste0(filenames_out, "_classified", ".png")
      
      # filenames of input images
      filenames_out <- paste0(filenames_out, ".png")
      
      
    } else {
      filenames_out <- paste0(seq(1, length(images_from_prediction[[output]])), ".png")
      filenames_out_class <- paste0(seq(1, length(images_from_prediction[[output]])), "_classified.png")
    }
    
    
    for(i in 1:length(images_from_prediction[[output]])){
      
      if(returnInput){
        magick::image_write(image = images_from_prediction$image[i],
                            path = file.path(dirOutput, filenames_out[i]))
      }
      
      magick::image_write(image = images_from_prediction[[output]][i],
                          path = file.path(dirOutput, filenames_out_class[i]))
    }
  }
  
  
  if(n_class == 1){
    image_summary <- data.frame(not_predicted = mean_not_predicted,
                                predicted = mean_predicted
                               )
  }

  if(n_class > 1) {
    image_summary <- prediction_percentages3
  }

  if(exists("info_df")){
    images_from_prediction$summary <- cbind(info_df,
                                            image_summary)
  } else {
    images_from_prediction$summary <- image_summary
  }


  return(images_from_prediction)
}
