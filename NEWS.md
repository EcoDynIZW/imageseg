# imageseg 0.5.1

## bugfixes
* imageSegmentation: fixed map_depth() error introduced with purrr >1.0.0


# imageseg 0.5.0

## new functions
* u_net_plusplus: Creates a U-Net++ architecture

## new features
* dataAugmentation / imagesToKerasInput: new argument "subset" (for subsetting function input)
* imagesToKerasInput: new argument "max" (to specify maximum value of image color range)
* resizeImages: new argument "filter", to specify filter algorithm for resizing.

## other changes
* u_net uses layer_conv_2d_transpose instead of layer_upsampling_2d
* updated vignette, better test/train/validation split

# imageseg 0.4.0

* initial version
