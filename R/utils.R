# bundle custom objects in list
# for easy loading as custom objects with model
# ADD PACKAGENAME::: ?
#' @importFrom keras layer_conv_2d
#' @importFrom keras keras_model
#' @importFrom keras layer_activation_relu
#' @importFrom keras layer_batch_normalization
#' @importFrom keras k_cast
#' @importFrom keras k_epsilon
#' @importFrom keras k_flatten
#' @importFrom keras k_greater
#' @importFrom keras k_sum
#' @importFrom keras loss_binary_crossentropy
#' @importFrom magick image_read
#' @importFrom magick image_orient



restoreCustomObjects <- function(){
  custom_objects <- list(dice_coef = dice_coef,
                         jaccard_index = jaccard_index,
                         bce_dice_loss = bce_dice_loss,
                         dice_coef_loss = dice_coef_loss)
  return(custom_objects)
}



imageRead <- function(filename) {

  img <- magick::image_read(filename)
  # assign orientation automatically
  img <- magick::image_orient(img)

  #return(list(img = img))
  return(img)
}


# #' Creates a double convolutional U-Net block.
u_net_double_conv2d <- function(input,
                                filters,
                                kernel_size,
                                batch_normalization = TRUE,
                                kernel_initializer = "he_normal") {
  input %>%
    layer_conv_2d(filters = filters,
                  kernel_size = kernel_size,
                  padding = "same",
                  kernel_initializer = kernel_initializer) %>%
    when(batch_normalization ~ layer_batch_normalization(.), ~ .) %>%
    layer_activation("relu") %>%
    
    layer_conv_2d(filters = filters,
                  kernel_size = kernel_size,
                  padding = "same",
                  kernel_initializer = kernel_initializer) %>%
    when(batch_normalization ~ layer_batch_normalization(.), ~ .) %>%
    layer_activation("relu")
}

# #' Creates a triple convolutional U-Net block.
u_net_triple_conv2d <- function(input,
                                filters,
                                kernel_size,
                                batch_normalization = TRUE,
                                kernel_initializer = "he_normal") {
  input %>%
    layer_conv_2d(filters = filters, kernel_size = kernel_size,
                  padding = "same", kernel_initializer = kernel_initializer) %>%
    when(batch_normalization ~ layer_batch_normalization(.), ~ .) %>%
    layer_activation("relu") %>%
    
    layer_conv_2d(filters = filters, kernel_size = kernel_size,
                  padding = "same", kernel_initializer = kernel_initializer) %>%
    when(batch_normalization ~ layer_batch_normalization(.), ~ .) %>%
    layer_activation("relu") %>%
    
    layer_conv_2d(filters = filters, kernel_size = kernel_size,
                  padding = "same", kernel_initializer = kernel_initializer) %>%
    when(batch_normalization ~ layer_batch_normalization(.), ~ .) %>%
    layer_activation("relu")
}



# Dice coefficient: input for loss function
# from: https://keras.rstudio.com/articles/examples/unet.html
dice_coef <- function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  result <- (2 * intersection + smooth) /
    (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
  return(result)
}


# adapted from:
# https://github.com/gschramm/pymirc/blob/b49dfd279f0f7a98549d5a239c4b558736ff0dbb/tutorial/utils/metrics.py
# can probably remove thresholding
jaccard_index <- function(y_true, y_pred){
  threshold = 0.5
  y_true = k_cast(k_greater(y_true, threshold), 'float32')
  y_pred = k_cast(k_greater(y_pred, threshold), 'float32')
  intersection = k_sum(y_true * y_pred)
  union = k_sum(y_true) + k_sum(y_pred)
  # avoid division by zero by adding 1
  jaccard = (intersection + k_epsilon()) / (union - intersection + k_epsilon())  # this contains as many elements as there are classes
  return (jaccard)
}




# Loss functions -----------------------------------------------------
# sum of binary_crossentropy and soft-Dice
bce_dice_loss <- function(y_true, y_pred) {
  result <- loss_binary_crossentropy(y_true, y_pred) +
    (1 - dice_coef(y_true, y_pred))
  return(result)
}

# Habitat net used negative Dice coefficient only
dice_coef_loss <- function(y_true, y_pred) {
  result <- - dice_coef(y_true, y_pred)
  return(result)
}



# circular weight matrix for even number of rows/cols
# adapted from raster:::..simple.circular.weight
circular.weight <- function (size,           # matrix size
                             ratio = 1) {   # ratio of circle diameter to matrix size


  if(ratio < 0) stop("'ratio must be between 0 and 1")
  if(ratio > 1) stop("'ratio must be between 0 and 1")

  diam <- round(size * ratio)

  # ensure diameter of inner circle is even
  if((diam %% 2) != 0) {
    if((diam + 1) <= size) {
      diam <- diam + 1
    } else {
      diam <- diam - 1
    }
  }

  x <- 1:diam           # coord along sides
  n <- length(x)        # number of rows/cols

  center <- diam/2 + 0.5  # center point (+ 0.5 so it is symmetrical for even number of rows/cols)

  d <- sqrt(rep(x - center, n)^2 + rep(x - center, each = n)^2) <= diam/2  # distance of each cell from circle center
  m <- matrix(d + 0, n, n)

  if(diam < size){
    cells_to_add <- (size-diam) / 2

    addrow <- matrix(0, nrow = cells_to_add, ncol = ncol(m))
    m <- rbind(addrow, m, addrow)

    addcol <- matrix(0, nrow =  nrow(m), ncol = cells_to_add)
    m <- cbind(addcol, m, addcol)
  }
  #image(m, asp = 1)
  return(m)
}
