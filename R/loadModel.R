#' Load TensorFlow model from hdf5 file
#' @importFrom keras load_model_hdf5
#' @param modelFile character. File name of the .hdf5 model file to load
#' @param restoreCustomObjects logical. Restore custom objects (loss function & dice coefficient) used in training of habitat models
#' @details Loads a trained TensorFlow model from a hdf5 file, and (optionally) restores custom objects.
#' @return keras model
#' @export
#'
#' @examples
#' \dontrun{
#' # Canopy model
#' wd_model_can <- "C:/Path/To/Model"      # change this
#' filename_model_can <- "imageseg_canopy_model.hdf5"
#' model_can <- loadModel(file.path(wd_model_can, filename_model_can))
#'
#' # Understory model
#' # note, here we just specify the complete path, not separate by directory and file name as above
#' model_file_us <- "C:/Path/To/Model/imageseg_understory_model.hdf5"
#' model_us <- loadModel(model_file_us)
#' }
loadModel <- function(modelFile, restoreCustomObjects = TRUE) {

  if(isTRUE(restoreCustomObjects)) {
  model <- load_model_hdf5(filepath = modelFile,
                           custom_objects = restoreCustomObjects())
  } else {
    model <- load_model_hdf5(filepath = modelFile)
  }

  return(model)
}
