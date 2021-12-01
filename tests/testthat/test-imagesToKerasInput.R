context("imagesToKerasInput")
library(imageseg)


# Run examples from imagesToKerasInput #

# Example 1: Canopy
wd_images_can <- system.file("images/canopy/resized",
                             package = "imageseg")
images_can <- loadImages(imageDir = wd_images_can)
x_can <- imagesToKerasInput(images_can)


wd_images_can_mask <- system.file("images/canopy/masks",
                             package = "imageseg")
images_can_mask <- loadImages(imageDir = wd_images_can_mask)
x_can_mask <- imagesToKerasInput(images = images_can_mask)

# Example 2: Understory
wd_images_us <- system.file("images/understory/resized",
                            package = "imageseg")
images_us <- loadImages(imageDir = wd_images_us)
x_us <- imagesToKerasInput(images_us)


# Test section #

test_that("object classes are correct", {
  expect_is(x_can, "array")
  expect_is(x_us, "array")
})

test_that("array dimensions are correct", {
  expect_equal(dim(x_can), c(11, 256, 256, 3))
  expect_equal(dim(x_can_mask), c(11, 256, 256, 1))
  expect_equal(dim(x_us) , c(3, 256, 160, 3))
})


test_that("data frame is correct", {
  expect_is(attr(x_can, "info"), "data.frame")
  expect_is(attr(x_us, "info"), "data.frame")
})

test_that("messages work", {
  expect_message(imagesToKerasInput(images_can), "11 images, 256 x 256 pixels, 3 color channels")
  expect_message(imagesToKerasInput(images_can_mask), "11 images, 256 x 256 pixels, 1 color channels")
  expect_message(imagesToKerasInput(images_us), "3 images, 160 x 256 pixels, 3 color channels")
})

