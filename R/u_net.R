#' Create a U-Net architecture
#' 
#' @description Create a U-Net architecture
#' 
#' @importFrom magrittr %>%
#' @importFrom purrr when
#'
#' @param net_h Input layer height.
#' @param net_w Input layer width.
#' @param grayscale Defines input layer color channels - 1 if `TRUE`, 3 if `FALSE`.
#' @param layers_per_block Number of convolutional layers per block (can be 2 or 3)
#' @param blocks Number of blocks in the model.
#' @param n_class Number of classes.
#' @param filters Integer, the dimensionality of the output space (i.e. the number of output filters in the convolution).
#' @param dropout Dropout rate (between 0 and 1).
#' @param batch_normalization Should batch normalization be used in the block?
#' @param kernel_initializer Initializer for the kernel weights matrix.
#' 
#' @return U-Net model.
#' @export
#'
#' @details This function creates a U-Net model architecture according to user input. It allows flexibility regarding input, output and the hidden layers. 
#' See the package vignette for examples . #' 
#' 
#' The function was adapted and slightly modified from the u_net() function in the platypus package (\url{https://github.com/maju116/platypus/blob/master/R/u_net.R}).
#'
#' Differences compared to platypus implementation:
#'
#' - added argument: layers_per_block (can be 2 or 3)  \cr
#' - kernel size in layer_conv_2d_transpose is 2, not 3.  \cr
#' - dropout layers are only included if user specifies dropout > 0 \cr
#' - n_class = 1 by default (sufficient for binary classification used for vegetation model, e.g. sky or not sky)  \cr
#' - activation of output layer: "sigmoid" if n_class = 1, otherwise "softmax" \cr
#' - no checks: allows non-square input images (e.g. 160x256 used in understory vegetation density model)  \cr
#'
#'
#' @return A keras model as returned by \code{\link[keras]{keras_model}}
#' 
#' @examples
#' \dontrun{
#' # U-Net model for 256x256 pixel RGB input images with a single output class
#' # this model was used for canopy density
#' 
#' model <- u_net(net_h = 256, 
#' net_w = 256, 
#' grayscale = FALSE,
#' filters = 32,
#' blocks = 4,
#' layers_per_block = 2
#' )
#' 
#' # several arguments above were not necessary because they were kept at their default. 
#' # Below is the same model, but shorter:
#' 
#' model <- u_net(net_h = 256, 
#' net_w = 256, 
#' filters = 32
#' )
#' 
#' model
#' 
#' }
#' 

u_net <- function(net_h,
                  net_w,
                  grayscale = FALSE,
                  layers_per_block = 2,
                  blocks = 4,
                  n_class = 1,
                  filters = 16,
                  dropout = 0,
                  batch_normalization = TRUE,
                  kernel_initializer = "he_normal") {

  layers_per_block <- match.arg(as.character(layers_per_block), choices = c("2", "3"))
  channels <- if (grayscale) 1 else 3
  input_shape <- c(net_h, net_w, channels)

  input_img <- layer_input(shape = input_shape,
                           name = 'input_img')

  conv_layers <- pool_layers <- conv_tr_layers <- list()


  if(layers_per_block == 2) {
    for (block in 1:blocks) {
      current_input <- if (block == 1) input_img else pool_layers[[block - 1]]
      conv_layers[[block]] <- u_net_double_conv2d(input = current_input,
                                                  filters = filters * 2^(block - 1),
                                                  kernel_size = 3,
                                                  batch_normalization = batch_normalization,
                                                  kernel_initializer = kernel_initializer)

      pool_layers[[block]] <- layer_max_pooling_2d(conv_layers[[block]], pool_size = 2)
      if(dropout != 0) pool_layers[[block]] <- pool_layers[[block]] %>% layer_dropout(rate = dropout)
    }


    conv_layers[[blocks + 1]] <- u_net_double_conv2d(input = pool_layers[[blocks]],
                                                     filters = filters * 2^blocks, kernel_size = 3,
                                                     batch_normalization = batch_normalization,
                                                     kernel_initializer = kernel_initializer)

    for (block in 1:blocks) {

      # platypus: layer_conv_2d_transpose
      # conv_tr_layers[[block]] <- layer_conv_2d_transpose(object = conv_layers[[blocks + block]],
      #                                                    filters = filters * 2^(blocks - block),
      #                                                    kernel_size = 2, ###     kernel_size = 3,
      #                                                    strides = 2,
      #                                                    padding = "same")

      # habitatnet / imageseg: layer_upsampling_2d (also below)
      conv_tr_layers[[block]] <- layer_upsampling_2d(object = conv_layers[[blocks + block]],
                                                    size = c(2, 2))


      conv_tr_layers[[block]] <- layer_concatenate(inputs = list(conv_layers[[blocks - block + 1]], conv_tr_layers[[block]]))
      if(dropout != 0) conv_tr_layers[[block]] <- conv_tr_layers[[block]] %>% layer_dropout(rate = dropout)


      conv_layers[[blocks + block + 1]] <- u_net_double_conv2d(input = conv_tr_layers[[block]],
                                                               filters = filters * 2^(blocks - block),
                                                               kernel_size = 3,
                                                               batch_normalization = batch_normalization,
                                                               kernel_initializer = kernel_initializer)
    }
  } else {
    for (block in 1:blocks) {
      current_input <- if (block == 1) input_img else pool_layers[[block - 1]]
      conv_layers[[block]] <- u_net_triple_conv2d(input = current_input,
                                                  filters = filters * 2^(block - 1),
                                                  kernel_size = 3,
                                                  batch_normalization = batch_normalization,
                                                  kernel_initializer = kernel_initializer)

      pool_layers[[block]] <- layer_max_pooling_2d(conv_layers[[block]], pool_size = 2)
      
      if(dropout != 0) pool_layers[[block]] <- pool_layers[[block]] %>% layer_dropout(rate = dropout)
    }

    conv_layers[[blocks + 1]] <- u_net_triple_conv2d(input = pool_layers[[blocks]],
                                                     filters = filters * 2^blocks,
                                                     kernel_size = 3,
                                                     batch_normalization = batch_normalization,
                                                     kernel_initializer = kernel_initializer)

    for (block in 1:blocks) {

      conv_tr_layers[[block]] <- layer_upsampling_2d(object = conv_layers[[blocks + block]],
                                                     size = c(2, 2))


      conv_tr_layers[[block]] <- layer_concatenate(inputs = list(conv_tr_layers[[block]], conv_layers[[blocks - block + 1]]))
      
      if(dropout != 0) conv_tr_layers[[block]] <- conv_tr_layers[[block]] %>% layer_dropout(rate = dropout)

      conv_layers[[blocks + block + 1]] <- u_net_triple_conv2d(input = conv_tr_layers[[block]],
                                                               filters = filters * 2^(blocks - block),
                                                               kernel_size = 3,
                                                               batch_normalization = batch_normalization,
                                                               kernel_initializer = kernel_initializer)
    }
  }

  output <- layer_conv_2d(object = conv_layers[[2 * blocks + 1]],
                          filters = n_class,
                          kernel_size = c(1, 1),
                          activation = ifelse(n_class == 1, "sigmoid", "softmax")    # softmax in platypus didn't learn for 1-class case
                          )

  keras_model(inputs = input_img,
              outputs = output)
}


