\name{imageseg-package}
\alias{imageseg-package}
\alias{imageseg}

\docType{package}

\title{
Overview of the imageseg package
}
\description{
This package provides a streamlined workflow for image segmentation using deep learning models based on the U-Net architecture. Image segmentation is the labelling of each pixel in a images with class labels. wModels are convolutional neural networks implemented in \pkg{keras} using a TensorFlow backend. The general workflow supports grayscale and color images as input, and single or multiple output classes.

We provide pre-trained models for two forest structural metrics:  canopy density and understory vegetation density. These trained models were trained with large and diverse training data sets to date, allowing for robust inferences. The package workflow is implemented in a few function, allowing for simple predictions on your own images without specialist knowledge.. 

If you have training data available, you can also create and train your own models, or continue model training on the pre-trained models.

The workflow implemented here can also be used for other image segmentation tasks, e.g. in the cell biology or for medical images. We provide two examples in the package vignette (bacteria detection in darkfield microscopy from color images, breast cancer detection in grayscale ultrasound images).

}


\section{Functions for model predictions}{

The following functions are used to perform image segmentation on your images. They resize images, load them into R, convert them to model input, load the model and perform predictions. The functions are given in the order they would typically be run.

\tabular{ll}{
\code{\link{resizeImages}} \tab Resize and save images \cr
\code{\link{loadImages}} \tab  Load image files with magick \cr
\code{\link{findValidRegion}} \tab Subset image to valid (informative) region (optional) \cr

\code{\link{imagesToKerasInput}} \tab Convert magick images to array for keras \cr
\code{\link{loadModel}} \tab Load TensorFlow model from hdf5 file \cr
\code{\link{imageSegmentation}} \tab Model predictions from images based on TensorFlow model \cr

}
}

\section{Functions for model training}{

This function assist in creating models in keras based on the U-Net architecture
\tabular{ll}{
\code{\link{dataAugmentation}} \tab Rotating and mirroring images, and modulating colors \cr
\code{\link{u_net}}  \tab Create a U-Net architecture \cr

}
}

\section{Download pre-trained models}{

Links to the pre-trained models, example classifications and all training data used can be found in the readme under

https://github.com/EcoDynIZW/imageseg

or in the package vignette

}


\section{Sample data in the package}{

The package contains sample data for testing the functions. Specifically, there are raw data (in original file size), resized images and masks (for both canopy and understory vegetation).

See \code{example(function)} or the "Examples" section at the bottom of the respective help file.

}



\section{Vignette}{

The package contains a pdf vignette demonstrating the workflow for predictions and model training. It covers Installation and setup, model predictions and training the forest structural models, and two more general applications. See \code{browseVignettes(package = "imageseg")}.

}


\author{
Juergen Niedballa, Jan Axtner

\bold{Maintainer}: Juergen Niedballa <niedballa@izw-berlin.de>
}

\references{

Abrams, J. F., Vashishtha, A., Wong, S. T., Nguyen, A., Mohamed, A., Wieser, S., ... & Mukhopadhyay, A. (2019). Habitat-Net: Segmentation of habitat images using deep learning. Ecological informatics, 51, 121-128. \url{https://www.sciencedirect.com/science/article/abs/pii/S1574954118302759} \cr


Ronneberger O., Fischer P., Brox T. (2015) U-Net: Convolutional Networks for Biomedical Image Segmentation. In: Navab N., Hornegger J., Wells W., Frangi A. (eds) Medical Image Computing and Computer-Assisted Intervention – MICCAI 2015. MICCAI 2015. Lecture Notes in Computer Science, vol 9351. Springer, Cham.
\doi{10.1007/978-3-319-24574-4_28}

}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{package}
\seealso{
\pkg{keras}
\pkg{tensorflow}
\pkg{magick}
}

