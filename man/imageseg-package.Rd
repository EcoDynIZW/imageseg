\name{imageseg-package}
\alias{imageseg-package}
\alias{imageseg}

\docType{package}

\title{
Overview of the imageseg package
}
\description{
This package provides a streamlined workflow for image segmentation using deep learning models based on the U-Net architecture by Ronneberger (2015) and the U-Net++ architecture by Zhou et al. (2018). Image segmentation is the labelling of each pixel in a images with class labels. Models are convolutional neural networks implemented in \pkg{keras} using a TensorFlow backend. The workflow supports grayscale and color images as input, and binary or multi-class output.

We provide pre-trained models for two forest structural metrics: canopy density and understory vegetation density. These trained models were trained with large and diverse training data sets, allowing for robust inferences. The package workflow is implemented in a few function, allowing for simple predictions on your own images without specialist knowledge of convolutional neural networks.

If you have training data available, you can also create and train your own models, or continue model training on the pre-trained models.

The workflow implemented here can also be used for other image segmentation tasks, e.g. in the cell biology or for medical images. We provide two examples in the package vignette (bacteria detection in darkfield microscopy from color images, breast cancer detection in grayscale ultrasound images).
}


\section{Functions for model predictions}{

The following functions are used to perform image segmentation on your images. They resize images, load them into R, convert them to model input, load the model and perform predictions. The functions are given in the order they would typically be run. See the vignette for complete examples.

\tabular{ll}{
\code{\link{findValidRegion}} \tab Subset image to valid (informative) region (optional) \cr
\code{\link{resizeImages}} \tab Resize and save images \cr
\code{\link{loadImages}} \tab  Load image files with magick \cr

\code{\link{imagesToKerasInput}} \tab Convert magick images to array for keras \cr
\code{\link{loadModel}} \tab Load TensorFlow model from hdf5 file \cr
\code{\link{imageSegmentation}} \tab Model predictions from images based on TensorFlow model \cr

}
}

\section{Functions for model training}{

This function assist in creating models in keras based on the U-Net architecture. See the vignette for complete examples.

\tabular{ll}{
\code{\link{dataAugmentation}} \tab Rotating and mirroring images, and modulating colors \cr
\code{\link{u_net}}  \tab Create a U-Net architecture \cr
\code{\link{u_net_plusplus}}  \tab Create a U-Net++ architecture \cr
}
}

\section{Download pre-trained models for forest structural metrics}{

Links to both pre-trained models (canopy and understory), example classifications and all training data used can be found in the GitHub readme under:

\url{https://github.com/EcoDynIZW/imageseg}

}



\section{Vignette}{

The package contains a pdf vignette demonstrating the workflow for predictions and model training using various examples. It covers installation and setup, model predictions and training the forest structural models, and two more general applications of image segmentation (multi-class image segmentation of RGB microscopy images, and single-class image segmentation of grayscale ultrasound breast scan images). See \code{browseVignettes(package = "imageseg")}.

}


\author{
Juergen Niedballa, Jan Axtner

\bold{Maintainer}: Juergen Niedballa <niedballa@izw-berlin.de>
}

\references{


Ronneberger O., Fischer P., Brox T. (2015) U-Net: Convolutional Networks for Biomedical Image Segmentation. In: Navab N., Hornegger J., Wells W., Frangi A. (eds) Medical Image Computing and Computer-Assisted Intervention – MICCAI 2015. MICCAI 2015. Lecture Notes in Computer Science, vol 9351. Springer, Cham.
\doi{10.1007/978-3-319-24574-4_28}

Zhou, Z., Rahman Siddiquee, M. M., Tajbakhsh, N., & Liang, J. (2018). Unet++: A nested u-net architecture for medical image segmentation. In Deep learning in medical image analysis and multimodal learning for clinical decision support (pp. 3-11). Springer, Cham.
\doi{10.48550/arXiv.1807.10165}

}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{package}
\seealso{
\pkg{keras}
\pkg{tensorflow}
\pkg{magick}
}

