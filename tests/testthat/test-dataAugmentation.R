context("dataAugmentation")
library(imageseg)


# Run examples from dataAugmentation #


# Example 1: Canopy
wd_images_can <- system.file("images/canopy/resized",
                             package = "imageseg")

images_can <- loadImages(imageDir = wd_images_can)

images_can_aug <- dataAugmentation(images = images_can,
                                   rotation_angles = c(0, 90, 180, 270),
                                   flip = TRUE,
                                   flop = TRUE)



# Test section #

test_that("object classes are correct", {
  expect_is(images_can_aug, "list")
  expect_is(images_can_aug$info, "data.frame")
  expect_is(images_can_aug$img, "magick-image")
})

test_that("output size is correct number", {
  expect_true(length(images_can_aug$img) == length(list.files(wd_images_can)) * 6)
})


test_that("data frame is correct", {
  expect_true(nrow(images_can_aug$info) == length(list.files(wd_images_can)) * 6)

  expect_equal(table(images_can_aug$info$rotation), structure(c(`0` = 33L, `90` = 11L, `180` = 11L, `270` = 11L), .Dim = 4L, .Dimnames = structure(list(
    c("0", "90", "180", "270")), .Names = ""), class = "table"))
  expect_equal(table(images_can_aug$info$flip), structure(c(`FALSE` = 55L, `TRUE` = 11L), .Dim = 2L, .Dimnames = structure(list(
    c("FALSE", "TRUE")), .Names = ""), class = "table"))
  expect_equal(table(images_can_aug$info$flop), structure(c(`FALSE` = 55L, `TRUE` = 11L), .Dim = 2L, .Dimnames = structure(list(
    c("FALSE", "TRUE")), .Names = ""), class = "table"))

})

test_that("error messages work", {

  expect_error(dataAugmentation(images = images_can,
                                rotation_angles = 0,
                                fraction_random_BSH  = 2),
               "fraction_random_BSH must be between 0 and 1")

  expect_error(dataAugmentation(images = images_can,
                                rotation_angles = 0,
                                fraction_random_BSH  = -2),
               "fraction_random_BSH must be postive")

})

test_that("warnings work", {

  expect_warning(dataAugmentation(images = images_can,
                                  rotation_angles = c(0, 45)),
                 "Some rotation_angles are not multiples of 90. These rotated images will have different dimensions than the originals and might not be valid model input.")


expect_warning(dataAugmentation(images = images_can,
                                rotation_angles = 90),
               "0 is not in rotation_angles. That will remove the original images from the augmented image set")

})
