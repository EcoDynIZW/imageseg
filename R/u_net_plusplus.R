
#' Create a U-Net++ architecture
#'
#' @description Create a U-Net++ architecture. 
#' 
#' @param net_h Input layer height.
#' @param net_w Input layer width.
#' @param grayscale Defines input layer color channels - 1 if `TRUE`, 3 if `FALSE`.
#' @param blocks Number of blocks in the model.
#' @param n_class Number of classes.
#' @param filters Integer, the dimensionality of the output space (i.e. the number of output filters in the convolution).
#' @param kernel_initializer Initializer for the kernel weights matrix.
#'
#' @details The function was ported to R from Python code in https://github.com/albertsokol/pneumothorax-detection-unet/blob/master/models.py. For more details, see https://github.com/MrGiovanni/UNetPlusPlus.
#'
#' @return  U-Net++ model.
#' @export
#'
#' @examples
#' \dontrun{
#' # U-Net++ model for 256x256 pixel RGB input images with a single output class
#' 
#' model <- u_net_plusplus(net_h = 256, 
#' net_w = 256, 
#' filters = 32,
#' blocks = 3 
#' )
#'  
#' model
#' 
#' }
#' 
u_net_plusplus <- function( net_h,
                            net_w,
                            grayscale = FALSE,
                            blocks = 4,
                            n_class = 1,
                            filters = 16,
                            # dropout = 0,
                            # batch_normalization = TRUE,
                            kernel_initializer = "he_normal") {
  
  
  if(blocks < 1) stop("block must be 1 or higher")
  if(blocks > 4) stop("block must be 4 or less")
  
  kernel_size <- 3
  
  channels <- if (grayscale) {1} else {3}
  input_shape <- c(net_h, net_w, channels)
  input_img <- layer_input(shape = input_shape, name = "input_img")
  
  conv0_0 <- down_conv_block(input = input_img, filter_mult = 1, filters = filters, kernel_size = 3, name = "conv0_0")
  pool0_0 <- conv0_0 %>% layer_max_pooling_2d(pool_size  = c(2, 2))
  conv1_0 <- pool0_0 %>% down_conv_block( filter_mult = 2, filters = filters, kernel_size = 3, name='conv1_0')
  # if(dropout != 0) conv1_0 <- conv1_0 %>% layer_dropout(rate = dropout)
  
  
  # Second stage
  if (blocks > 1) {
    pool1_0 <- conv1_0 %>% layer_max_pooling_2d(pool_size  = c(2, 2))
    conv2_0 <- pool1_0 %>% down_conv_block(filter_mult = 4, filters = filters, kernel_size = kernel_size, name='conv2_0')
  }
  
  # Third stage
  if (blocks > 2){
    pool2_0 <- conv2_0 %>% layer_max_pooling_2d(pool_size  = c(2, 2))
    conv3_0 <- pool2_0 %>% down_conv_block(filter_mult = 8, filters = filters, kernel_size = kernel_size, name='conv3_0')
  }
  
  # Fourth stage
  if (blocks > 3){
    pool3_0 <- conv3_0 %>% layer_max_pooling_2d(pool_size  = c(2, 2))
    # conv4_0 = down_conv_block(pool3_0, 16, filters, kernel_size, name='conv4_0')
    conv4_0 <- pool3_0 %>% down_conv_block(filter_mult = 16, filters = filters, kernel_size = kernel_size, name='conv4_0')
  }
  
  
  # First stage of upsampling and skip connections
  # note, changed the order of input and prev, compared to template
  conv0_1 <- up_conv_block(input = conv1_0, prev = conv0_0, filter_mult = 1, filters = filters, kernel_size = kernel_size, name='conv0_1')
  out <- conv0_1
  
  
  # Second stage
  if (blocks > 1) {
    conv1_1 <- up_conv_block(input = conv2_0, prev = conv1_0, filter_mult = 2, filters = filters, kernel_size = kernel_size, name='conv1_1')
    conv0_2 <- up_conv_block(input = conv1_1, prev = conv0_1, filter_mult = 1, filters = filters, kernel_size = kernel_size, prev_2=conv0_0, name='conv0_2')
    out <- conv0_2
  }
  
  # Third stage
  if (blocks > 2) {
    conv2_1 <- up_conv_block(conv3_0, conv2_0, 4, filters, kernel_size, name='conv2_1')
    conv1_2 <- up_conv_block(conv2_1, conv1_1, 2, filters, kernel_size, prev_2=conv1_0, name='conv1_2')
    
    conv0_3 <- up_conv_block(conv1_2, conv0_2, 1, filters, kernel_size, prev_2=conv0_1, prev_3=conv0_0,
                             name='conv0_3')
    out <- conv0_3
  }
  
  # Fourth stage
  if (blocks > 3) {
    conv3_1 <- up_conv_block(conv4_0, conv3_0, 8, filters, kernel_size, name='conv3_1')
    conv2_2 <- up_conv_block(conv3_1, conv2_1, 4, filters, kernel_size, prev_2=conv2_0, name='conv2_2')
    conv1_3 <- up_conv_block(conv2_2, conv1_2, 2, filters, kernel_size, prev_2=conv1_1, prev_3=conv1_0,
                             name='conv1_3')
    conv0_4 = up_conv_block(conv1_3, conv0_3, 1, filters, kernel_size, prev_2=conv0_2, prev_3=conv0_1,
                            prev_4=conv0_0, name='conv0_4')
    out <- conv0_4
  }
  
  
  output <- layer_conv_2d(object = out,
                          filters = n_class,
                          kernel_size = c(1, 1),
                          activation = ifelse(n_class == 1, "sigmoid", "softmax")
  )
  
  # return(out)
  keras_model(inputs = input_img,
              outputs = output)
  
}


down_conv_block <- function(input, 
                            filter_mult, 
                            filters, 
                            kernel_size, 
                            kernel_initializer = "he_normal",
                            name=NULL){
  input %>% 
    layer_conv_2d(filters = filter_mult * filters, 
                  kernel_size = kernel_size, 
                  padding='same', 
                  activation='relu',
                  kernel_initializer = kernel_initializer
    ) %>%
    # when(batch_normalization ~ layer_batch_normalization(.), ~ .) %>%
    # layer_activation("relu") %>%
    layer_batch_normalization() %>%
    
    layer_conv_2d(filters = filter_mult * filters, 
                  kernel_size = kernel_size, 
                  padding='same', 
                  activation='relu',
                  kernel_initializer = kernel_initializer
    ) %>%
    layer_batch_normalization(name = name)
  # when(batch_normalization ~ layer_batch_normalization(.), ~ .) # %>%
  # layer_activation("relu")
  
}



up_conv_block <- function(input, prev, filter_mult, filters, kernel_size, 
                          prev_2=NULL, prev_3=NULL, prev_4=NULL, 
                          kernel_initializer = "he_normal", name=NULL){
  
  m_tmp <- input %>% 
    layer_conv_2d_transpose(filter_mult * filters, 
                            kernel_size =  kernel_size, 
                            strides= c(2, 2), 
                            padding='same', 
                            activation='relu') %>%
    layer_batch_normalization()
  
  # Concatenate layers; varies between UNet and UNet++
  if (!is.null(prev_4)) {
    m_tmp2 <- layer_concatenate(list(m_tmp, prev, prev_2, prev_3, prev_4))
  } else {
    if (!is.null(prev_3)) {
      m_tmp2 <- layer_concatenate(list(m_tmp, prev, prev_2, prev_3))
    } else {
      if (!is.null(prev_2)) {
        m_tmp2 <- layer_concatenate(list(m_tmp, prev, prev_2))
      } else {
        m_tmp2 <- layer_concatenate(list(m_tmp, prev))
      }
    }
  }
  
  m_tmp2 %>% layer_conv_2d(filter_mult * filters, 
                           kernel_size = kernel_size, 
                           padding='same', 
                           activation='relu',
                           kernel_initializer = kernel_initializer) %>%
    layer_batch_normalization(name=name)
  
  return (m_tmp2)
}


