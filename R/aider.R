
# R CMD check notes configuration -----------------------------------------

## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c(".", ".data"), add = TRUE)
