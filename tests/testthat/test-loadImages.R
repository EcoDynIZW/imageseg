context("loadImages")
library(imageseg)


# Run examples from loadImages #

# Example 1: Canopy
wd_images_can <- system.file("images/canopy/resized",
                             package = "imageseg")

images_can <- loadImages(imageDir = wd_images_can)

# Example 2: Understory
wd_images_us <- system.file("images/understory/resized",
                            package = "imageseg")
images_us <- loadImages(imageDir = wd_images_us)



# Test section #

test_that("object classes are correct", {
  expect_is(images_can, "list")
  expect_is(images_us, "list")
  expect_is(images_can$info, "data.frame")
  expect_is(images_us$info, "data.frame")
  expect_is(images_can$img, "magick-image")
  expect_is(images_us$img, "magick-image")
})

test_that("correct number of images was loaded", {
  expect_true(length(images_can$img) == length(list.files(wd_images_can)))
  expect_true(length(images_us$img) == length(list.files(wd_images_us)))
})


test_that("data frame is correct", {
  expect_true(nrow(images_can$info) == length(list.files(wd_images_can)))
  expect_true(nrow(images_us$info) == length(list.files(wd_images_us)))
})

test_that("messages work", {
  expect_message(loadImages(imageDir = wd_images_can), "found 11 images")
  expect_message(loadImages(imageDir = wd_images_us), "found 3 images")
})
