% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findValidRegion.R
\name{findValidRegion}
\alias{findValidRegion}
\title{Subset image to valid (informative) region}
\usage{
findValidRegion(image, fileName, threshold = 0.1)
}
\arguments{
\item{image}{magick image}

\item{fileName}{file name of image to load}

\item{threshold}{numeric. Minimum range (max - min) of grayscale values for rows/columns to be included in the output image.}
}
\value{
A list with 3 items:

* img: the magick image.
* geometry_area: a geometry string that can be used as argument \code{geometry} in \code{\link[magick]{image_crop}}.
* geometry_area_df: a data frame containing the information from geometry_area (can be useful for finding consensus are to crop from many images)
}
\description{
Load images, find and crop valid (informative) region. This function removes black borders from images, and is suitable for restricting hemispherical (fisheye) images to the actual informative region in the image center.
}
\details{
Images are converted to grayscale according to the formula in Li et al. (2020). L = 0.30R + 0.59G + 0.11B
We use a default threshold of 10, but it can be adjusted freely.

This function can optionally be called inside \code{\link{resizeImages}} to crop each image to the informative region before resizing to the dimensions expected by the models. It is not recommended though since it may return different crop masks for images and their masks.
}
\section{Warning}{

Depending on the quality of the photographic equipment used, supposedly dark regions of images may be affected by stray light and light diffraction. This will be especially prevalend when using fisheye adapters, e.g. for smartphones. In such cases, the function will not provide reliable output. Adjusting `threshold` may help to a degree, but needs to be set on a case-to-case basis for individual images. In such cases it might be easier to instead use e.g. GIMP to measure out the valid image region.
}

\examples{

wd_images_can <- system.file("images/canopy/raw",
                              package = "imageseg")
lf <- list.files(wd_images_can, full.names = TRUE)
img <- findValidRegion(fileName = lf[1])
img

# finding consensus among multiple images
\dontrun{
wd_with_many_images <- "..."
lf <- list.files(wd_with_many_images)
test <- lapply(lf, findValidRegion)
# combine geometry_area_df from many images
geometry_areas_df <- do.call(rbind, lapply(test, FUN = function(x) x$geometry_area_df))
# summary to decide on suitable values
summary(geometry_areas_df)
}
}
\references{
Li, Kexin, et al. "A New Method for Forest Canopy Hemispherical Photography Segmentation Based on Deep Learning." Forests 11.12 (2020): 1366.
}
