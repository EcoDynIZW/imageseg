# imageseg
R package for deep learning image segmentation using U-Net model architecture, implementated in Keras and TensorFlow. It provides pre-trained models for forest structural metrics (canopy density, and understory vegetation density).

In addition, it provides a workflow for easily creating model input and model architectures for general-purpose image segmentation, based on grayscale or color images, and providing binary or multi-class image segmentation.

# Installation

Install the package via:

``` r
library(remotes)
install_github("jniedballa/imageseg", build_vignettes = TRUE)
```

See the vignette for information about setting up the Keras and TensorFlow. 


