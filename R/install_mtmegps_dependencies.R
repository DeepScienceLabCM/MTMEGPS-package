#' Install Python dependencies for MTMEGPS
#'
#' This function installs the required Python environment
#' for running deep learning models in MTMEGPS, including
#' TensorFlow and Keras3 through the reticulate interface.
#'
#' @return None
#' @export
install_mtmegps_dependencies <- function() {

  message("Checking for 'keras3'...")

  if (!requireNamespace("keras3", quietly = TRUE)) {
    stop("The R package 'keras3' is not installed. Please install it with: install.packages('keras3')")
  }

  message("Installing TensorFlow + Keras3 Python environment...")
  
  # this will create a virtualenv or conda env automatically
  keras3::install_keras()

  message("--------------------------------------------------------")
  message("✓ MTMEGPS Python dependencies installed successfully.")
  message("You may need to restart R before running deep learning models.")
  message("--------------------------------------------------------")
}
