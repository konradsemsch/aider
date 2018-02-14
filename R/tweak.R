
# First to lower ----------------------------------------------------------

#' Convert strings beginning to lowercase
#'
#' This function converts your data frame column names of vector elements first letter to lowercase.
#'
#' @param x A data frame or vector
#' @keywords names
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %<>%
#'   first_to_lower()
#' @export
first_to_lower <- function(x) {
  if (is.data.frame(x)) {
    df_names <- names(x)
    substr(df_names, 1, 1) <- tolower(substr(df_names, 1, 1))
    names(x) <- df_names
    x
  } else if (all(is.vector(x), is.character(x))) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  } else {
    stop("argument must be a data frame or character vector")
  }
}

# First to upper ----------------------------------------------------------

#' Convert strings beginning to uppercase
#'
#' This function converts your data frame column names of vector elements first letter to uppercase.
#'
#' @param x A data frame or vector
#' @keywords names
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %<>%
#'   first_to_lower()
#' @export
first_to_upper <- function(x) {
  if (is.data.frame(x)) {
    df_names <- names(x)
    substr(df_names, 1, 1) <- toupper(substr(df_names, 1, 1))
    names(x) <- df_names
    x
  } else if (all(is.vector(x), is.character(x))) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  } else {
    stop("argument must be a data frame or character vector")
  }
}

# Cap at percentile -------------------------------------------------------

#' Cap numeric values at selected percentiles
#'
#' This function capps values of a numeric vector at a specified percentile.
#' Can be used both with numeric vectors of data frames with mutate and map_at.
#'
#' @param x A vector or data frame column
#' @param floor A bottom percentile. Defaults to 0.975
#' @param roof A top percentile. Defaults to 0.025
#' @keywords cap
#' @examples
#' x <- seq(1, 100, 1)
#' cap_at_percentile(x)
#'
#' data <- data.frame(x = seq(1, 100, 1))
#' data %<>%
#'   mutate(y = cap_at_percentile(x))
#'
#' @export
cap_at_percentile <- function(x, floor = 0.025, roof = 0.975) {

  if (any(!is.numeric(x), !is.vector(x)))
    stop("argument must be a numeric vector")

  floor_cap <- quantile(x, floor)
  roof_cap  <- quantile(x, roof)

  y <- dplyr::case_when(
    x > roof_cap ~ roof_cap,
    x < floor_cap ~ floor_cap,
    TRUE ~ x)

  attributes(y) <- NULL
  y
}

# Cap between -------------------------------------------------------------

#' Cap numeric values between two values
#'
#' This function capps values of a numeric vector between specified values.
#' Can be used both with numeric vectors of data frames with mutate and map_at.
#'
#' @param x A vector or data frame column
#' @param floor A bottom number. Defaults to NA
#' @param roof A top number. Defaults to NA
#' @keywords cap
#' @examples
#' x <- seq(1, 100, 1)
#' cap_between(x, 40, 60)
#'
#' data <- data.frame(x = seq(1, 100, 1))
#'
#' data %<>%
#'   mutate(y = cap_between(x, 40, 60))
#'
#' @export
cap_between <- function(x, floor = NA, roof = NA) {

  if (any(!is.numeric(x), !is.vector(x)))
    stop("argument must be a numeric vector")

  floor_cap <- ifelse(is.na(floor), min(x), floor)
  roof_cap  <- ifelse(is.na(roof), max(x), roof)

  y <- dplyr::case_when(
    x > roof_cap ~ roof_cap,
    x < floor_cap ~ floor_cap,
    TRUE ~ x
  )
}

# Round to ----------------------------------------------------------------

#' Round to integers
#'
#' This function rounds values of a numeric vector to a selected integer, for example: 1,000 or 10,000.
#'
#' @param x A vector or data frame column
#' @param to An integer to round to. Defaults to a 1,000
#' @keywords round
#' @examples
#' round_to(12456)
#' @export
round_to <- function(x, to = 1000) {

  if (any(!is.numeric(x), !is.vector(x)))
    stop("argument must be a numeric vector")

  if (!is.numeric(to))
    stop("argument must be a numeric vector")

  round(x / to, 0) * to
}

# Format my table ---------------------------------------------------------

#' Format a table nicely
#'
#' This function creates nicely formatted tables in R markdown. It also works with the 'formattable' package.
#'
#' @param df A data frame
#' @param width Should the table have full-page width. Defaults to TRUE
#' @keywords table
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   calculate_share(country) %>%
#'   format_my_table()
#' @export

format_my_table <- function(df, width = TRUE) {

  df %>%
    knitr::kable(format = "html", digits = 3, align = "c", escape = FALSE) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = width,
                              position = "center")
}
