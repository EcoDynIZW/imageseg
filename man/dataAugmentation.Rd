% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataAugmentation.R
\name{dataAugmentation}
\alias{dataAugmentation}
\title{Data augmentation: rotating and mirroring images, and adjusting colors}
\usage{
dataAugmentation(
  images,
  subset = NULL,
  rotation_angles = 0,
  flip = FALSE,
  flop = FALSE,
  brightness_shift_lim = c(90, 110),
  saturation_shift_lim = c(95, 105),
  hue_shift_lim = c(80, 120),
  fraction_random_BSH = 0
)
}
\arguments{
\item{images}{list. Output of \code{loadImages}. List with two items ($info: data frame with information about images, $img: tibble containing magick images)}

\item{subset}{integer. Indices of images to process. Can be useful for only processing subsets of images (e.g. training images, not test/validation images).}

\item{rotation_angles}{integer. Angles in which to rotate images using \code{\link[magick]{image_rotate}})?}

\item{flip}{logical. mirror along horizontal axis (turn images upside-down using \code{\link[magick]{image_flip}})?}

\item{flop}{mirror along vertical axis (switch left and right) using \code{\link[magick]{image_flop}})?}

\item{brightness_shift_lim}{numeric. Lower and upper limits for argument \code{brightness} in \code{\link[magick]{image_modulate}}}

\item{saturation_shift_lim}{numeric. Lower and upper limits for argument \code{saturation} in \code{\link[magick]{image_modulate}}}

\item{hue_shift_lim}{numeric. Lower and upper limits for argument \code{hue} in \code{\link[magick]{image_modulate}}}

\item{fraction_random_BSH}{numeric. Fraction of images to apply random brightness / saturation / hue shifts to (between 0 and 1)}
}
\value{
A list with 2 elements: $info, a data frame with information about the images, and $img, a tibble with magick images
}
\description{
Rotate and/or mirror images to create augmented training data. Optionally, apply a random shift in brightness, saturation and hue to a certain percentage of the images
}
\details{
For creating training data for canopy, rotation and mirroring in both axes is appropriate. For understory vegetation density, only flop images (don't flip), and don't apply a hue shift since recognition of the orange flysheet is color-critical.
}
\examples{

# Example 1: Canopy
wd_images_can <- system.file("images/canopy/resized",
                             package = "imageseg")

images_can <- loadImages(imageDir = wd_images_can)

images_can_aug <- dataAugmentation(images = images_can,
                                   rotation_angles = c(0, 90, 180, 270),
                                   flip = TRUE,
                                   flop = TRUE)
images_can_aug

}
