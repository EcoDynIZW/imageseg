\name{deepForest-package}
\alias{deepForest-package}
\alias{deepForest}

\docType{package}

\title{
Overview of the deepForest package
}
\description{
This package provides a streamlined workflow for processing habitat images of canopy cover, canopy closure and understory vegetation density using deep learning models. We provide pre-trained models which allow for simple predictions on your own data. Models are convolutional neural networks based on the U-Net architecture, implemented in \pkg{keras} using a TensorFlow backend. Images are handled with the \pkg{magick} package. If you have training data available, you can also create and train your own models or continue model training on the pre-trained models.

The workflow we provide can potentially also be used for other image segmentation tasks, e.g. in the medical field.
}


\section{Functions for model predictions}{

The following functions are used to prepare images for processing with our pre-trained models. The resize images, load them into R, convert them to model input, load the model and perform predictions. The functions are given in the order they would typically be run.

\tabular{ll}{
\code{\link{resizeImages}} \tab Resize and save images \cr
\code{\link{loadImages}} \tab  Load image files with magick \cr
\code{\link{findValidRegion}} \tab Subset image to valid (informative) region (optional) \cr
\code{\link{dataAugmentation}} \tab Rotating and mirroring images, and modulating colors (optional) \cr
\code{\link{imagesToKerasInput}} \tab Convert magick images to array for keras \cr
\code{\link{loadModel}} \tab Load TensorFlow model from hdf5 file \cr
\code{\link{imageSegmentation}} \tab Model predictions from images based on TensorFlow model \cr

}
}

\section{Functions for model training}{

This function assist in creating models in keras based on the U-Net architecture
\tabular{ll}{
\code{\link{u_net}}  \tab Create a U-Net architecture \cr
}
}

\section{Download pre-trained models}{
We provide pre-trained models as hdf5 files. Download the models from:

Canopy model \url{https://www.dropbox.com/s/q4xztzzbmxzxwdy/deepForest_canopy_model.hdf5?dl=1}

Understory model: \url{https://www.dropbox.com/s/wb38mlb6agl09y0/deepForest_understory_model.hdf5?dl=1}

Save the files in a convenient location, and load them via \code{\link{loadModel}}.


There are also examples classifications based on these models available.

Canopy model examples \url{https://www.dropbox.com/sh/88udqtarvt6pu1a/AADHgN3C0TVt3QS7nwRlEvhga?dl=0}

Understory model examples: \url{https://www.dropbox.com/sh/t0zxleb1q7gt0al/AADsX7ie_tdL6q2-P3W1FNHWa?dl=0}

These classified images are part of the test image set. That means that the model has not seen these images and was not trained on them.

}


\section{Sample data}{

The package contains sample data for testing the functions. Specifically, there are raw data (in original file size), resized images, and masks (for both canopy and understory vegetation).

See \code{example(function)} or the "Examples" section at the bottom of the respective help file.

}



\section{Training and test data availability}{

The training and test data we used are available at:

Canopy training data  \url{https://www.dropbox.com/s/j96ncev8dxlr44y/canopy_training_data.zip?dl=0}

Understory training data  \url{https://www.dropbox.com/s/n2s5uqb4ber8nb1/understory_training_data.zip?dl=0}

Note the data are not split into test and training. We used a 80/20 split for training/test data.

}

\section{Vignettes}{

The package contains a pdf vignette demonstrating the workflow for predictions and model training. See \code{browseVignettes(package = "deepForest")}

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

