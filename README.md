# imageseg
R package for deep learning image segmentation using U-Net model architecture, implemented in Keras and TensorFlow. It provides pre-trained models for forest structural metrics (canopy density, and understory vegetation density) and a workflow to apply these on custom images.

In addition, it provides a workflow for easily creating model input and model architectures for general-purpose image segmentation, based on grayscale or color images, and providing binary or multi-class image segmentation.

# Installation

Install the package via:

``` r
library(remotes)
install_github("jniedballa/imageseg", build_vignettes = TRUE)
```

See the vignette for information about setting up the Keras and TensorFlow. 



# Where are the models?

The pre-trained models for forest canopy density and understory vegetation density are available for download:

**Canopy model**: https://www.dropbox.com/s/jalrvhmh7oo9g4n/imageseg_canopy_model.hdf5?dl=1

**Understory model**: https://www.dropbox.com/s/9qvgcc9j5r36spp/imageseg_understory_model.hdf5?dl=1

Please see the vignette for further information.
