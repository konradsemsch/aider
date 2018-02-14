
# Aider custom color schemes ----------------------------------------------

#' Use aider colors schemes
#'
#' This function creates different pallete vectors to be used for ggplots.
#'
#' @keywords pallete
#' @examples
#' get_aider_palletes()
#' @export
get_aider_palletes <- function() {

  aid_blue <- c("#03a0d8",
                "#1565b3",
                "#1d8fd2",
                "#36bd25",
                "#404040",
                "#d8ad03",
                "#F04E23",
                "#D71F26")

  assign("aid_blue", aid_blue, .GlobalEnv)

  aid_credit_grade <- c(
    "A.1" = "#00e500",
    "A.2" = "#00ff00",
    "A.3" = "#19ff19",
    "B.1" = "#32ff32",
    "B.2" = "#4cff4c",
    "B.3" = "#66ff66",
    "C.1" = "#7fff7f",
    "C.2" = "#99ff99",
    "C.3" = "#b2ffb2",
    "D.1" = "#FFFF66",
    "D.2" = "#FFFF00",
    "D.3" = "#FFCC00",
    "E.1" = "#F8AD45",
    "E.2" = "#FF9B19",
    "E.3" = "#F68313",
    "F.1" = "#F4675C",
    "F.2" = "#F44336",
    "F.3" = "#F02F24"
  )

  assign("aid_credit_grade", aid_credit_grade, .GlobalEnv)

  aid_risk <- c(
    "0" = "#40C157",
    "1" = "#F4675C",

    "Pl"  = "#40C157",
    "Npl" = "#F4675C",

    "Predicted" = "#34B1Df",
    "Actual"    = "#F4675C",
    "Target"    = "#F4675C",

    "Without BA model" = "#F4675C",
    "With BA model"    = "#00e500",

    "Approved" = "#40C157",
    "Rejected" = "#F4675C"
  )

  assign("aid_risk", aid_risk, .GlobalEnv)

  aid_deciles <- c(
    "1" = "#00e500",
    "2" = "#00ff00",
    "3" = "#19ff19",
    "4" = "#32ff32",
    "5" = "#b2ffb2",
    "6" = "#FFFF66",
    "7" = "#FFCC00",
    "8" = "#FF9B19",
    "9" = "#F4675C",
    "10" = "#F02F24"
  )

  assign("aid_deciles", aid_deciles, .GlobalEnv)

}
