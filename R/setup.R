
# Set me up ---------------------------------------------------------------

#' Basic R session setup
#'
#' This function sets up basic R parameters: seed, scipien and R tools path.
#'
#' @examples
#' set_me_up()
#' @export
set_me_up <- function() {

  set.seed(500)
  options(scipen = 100)
  # The latter depends on the operating system. It is not fully implemented at the moment
  # Sys.setenv(R_ZIPCMD = "C:/RBuildTools/3.4/bin/zip.exe")

}
