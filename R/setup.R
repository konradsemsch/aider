
# Set me up ---------------------------------------------------------------

#' Basic R session setup
#'
#' This function sets up basic R parameters: seed & scipien
#'
#' @examples
#' set_me_up()
#' @export
set_me_up <- function() {

  set.seed(500)
  options(scipen = 100)

}
