# imageseg

R package for deep learning image segmentation using the U-Net model architecture by Ronneberger (2015), implemented in Keras and TensorFlow. It provides pre-trained models for forest structural metrics (canopy density and understory vegetation density) and a workflow to apply these on custom images.

In addition, it provides a workflow for easily creating model input and model architectures for general-purpose image segmentation based on the U-net architecture. Model can be trained on grayscale or color images, and can provide binary or multi-class image segmentation as output.

The package can be found on CRAN: 

https://cran.r-project.org/web/packages/imageseg/index.html

The preprint of the paper describing the package is available on bioRxiv:

https://doi.org/10.1101/2021.12.16.469125


# Installation

First, install the R package "R.rsp" which enables the static vignettes. 

``` r
install.packages(R.rsp)
``` 

Install the imageseg package from CRAN via:

``` r
install.packages(imageseg)
``` 

Alternatively you can install from GitHub (requires remotes package and R.rsp):

``` r
library(remotes)   
install_github("EcoDynIZW/imageseg", build_vignettes = TRUE)
```

Using imageseg requires Keras and TensorFlow. See the vignette for information about installation and initial setup:

# Tutorial

See the vignette for an introduction and tutorial to imageseg.

``` r
browseVignettes("imageseg")
```

The vignette covers:

* Installation and setup
* Sample workflow for canopy density assessments
* Training new models
* Continued training of existing models
* Multi-class image segmentation models
* Image segmentation based on grayscale images



# Forest structure model download

The **pre-trained models** for forest canopy density and understory vegetation density are available for download:

*Canopy model*: https://www.dropbox.com/s/rtsly7kfag9fzlh/imageseg_canopy_model.hdf5?dl=1

*Understory model*: https://www.dropbox.com/s/9qvgcc9j5r36spp/imageseg_understory_model.hdf5?dl=1

Please see the vignette for further information.

**Example classifications** to give you an impression of model performance:

*Canopy model examples*  https://www.dropbox.com/sh/ypxx5rknpgqolxk/AAATyhQ8-wIi5I9aGlekqn7ia?dl=0

*Understory model examples* https://www.dropbox.com/sh/4gngdvk7km92clp/AAC2EtoB7lZiQefWVIwFiWZha?dl=0


# Training data download

*Canopy training data*  https://www.dropbox.com/s/302yyoi7qil1hn5/canopy_training_data_imageseg.zip?dl=1

*Understory training data*  https://www.dropbox.com/s/s7o7x66l3wiqc6h/understory_training_data_imageseg.zip?dl=1
