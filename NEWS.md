# imageseg 0.5.0

## new functions
* u_net_plusplus: Creates a U-Net++ architecture

## new features
* dataAugmentation / imagesToKerasInput: new argument "subset" (for subsetting function input)
* imagesToKerasInput: new argument "max" (to specify maximum value of image color range)

## other changes
* u_net uses layer_conv_2d_transpose instead of layer_upsampling_2d
* updated vignette, better test/train/validation split

# imageseg 0.4.0

* initial version
