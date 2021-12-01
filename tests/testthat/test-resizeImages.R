context("resizeImages")
library(imageseg)
library(magick)

# Run examples from resizeImages #
# Example 1: Canopy
wd_can <- system.file("images/canopy/raw",
                      package = "imageseg")

wd_out_can <- file.path(tempdir(), "canopy", "resized")
resizeImages(imageDir = wd_can,
             type = "canopy",
             outDir = wd_out_can)

filename_resized <- list.files(wd_out_can, full.names = TRUE)

# check output
img_can <- magick::image_read(filename_resized)

# Example 2: Understory
wd_us <- system.file("images/understory/raw",
                     package = "imageseg")
wd_out_us <- file.path(tempdir(), "understory", "resized")

# note, these are png images
resizeImages(imageDir = wd_us,
             type = "understory",
             outDir = wd_out_us)

filename_resized <- list.files(wd_out_us, full.names = TRUE)

# check output
img_us <- magick::image_read(filename_resized)



# Test section #

test_that("object classes are correct", {
  expect_is(wd_can, "character")
  expect_is(wd_out_can, "character")
  expect_is(wd_out_us, "character")
  expect_is(wd_us, "character")
  expect_is(img_can, "magick-image")
  expect_is(img_us, "magick-image")
})

test_that("correct number of images was extracted", {
  expect_true(length(img_can) == 3)
  expect_true(length(img_us) == 1)
})


file_can_resized_tmp <- list.files(wd_out_can, full.names = T)
img_can_resized <- imageseg:::imageRead(file_can_resized_tmp)


file_us_resized_tmp <- list.files(wd_out_us, full.names = T)
img_us_resized <- imageseg:::imageRead(file_us_resized_tmp)

test_that("resized images have correct dimensions", {
  expect_equal(unique(image_info(img_can_resized)$width),  256)
  expect_equal(unique(image_info(img_can_resized)$height), 256)

  expect_equal(unique(image_info(img_us_resized)$width),  160)
  expect_equal(unique(image_info(img_us_resized)$height), 256)

})


