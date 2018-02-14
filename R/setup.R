
# Set me up ---------------------------------------------------------------

#' Basic R setup
#'
#' This function sets up basic R parameters: seed, scipien, R tools path and loads the tidyverse library.
#'
#' @keywords setup
#' @examples
#' set_me_up()
#' @export
set_me_up <- function() {

  set.seed(500)
  options(scipen = 100)
  Sys.setenv(R_ZIPCMD = "C:/RBuildTools/3.4/bin/zip.exe")
  library(tidyverse)

}
