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

The models, example predictions, training data and R script for model training for both the canopy and understory model are available from Dryad as a single download:

https://doi.org/10.5061/dryad.x0k6djhnj


See the "Usage Notes" section for details on the dataset. 


The models and script (without the training data) are also hosted on Zenodo and can be downloaded individually from:

https://doi.org/10.5281/zenodo.6861157

The **pre-trained models** for forest canopy density and understory vegetation density are available for download. The zip files contain the model (as .hdf5 files) and **example classifications** to give an impression of model performance and output:

*Canopy model*: https://zenodo.org/record/6861157/files/imageseg_canopy_model.zip?download=1

*Understory model*: https://zenodo.org/record/6861157/files/imageseg_understory_model.zip?download=1


Please see the vignette for further information on how to use these models.



# Training data download

Training data for both the canopy and understory model are included in the Dryad dataset download in the zip files:

*imageseg_canopy_training_data.zip* 

*imageseg_understory_training_data.zip*

For details, please see the Usage Notes and the info.txt files contained in the zip files.

The training data are not required for users who only wish to use the pre-trained models on their own images.
