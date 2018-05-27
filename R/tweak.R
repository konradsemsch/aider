
# First to lower ----------------------------------------------------------

#' Convert strings first letter to lowercase
#'
#' This function converts your data frame column names or character vector elements
#' first letter to lowercase. It can be used in a pipe.
#'
#' @param x A data frame or vector
#' @examples
#' credit_data %<>%
#'   first_to_lower()
#'
#' first_to_lower(c("First", "Second"))
#' @export
first_to_lower <- function(x) {

  if (is.data.frame(x)) {
    df_names <- names(x)
    substr(df_names, 1, 1) <- tolower(substr(df_names, 1, 1))
    names(x) <- df_names
    x
  } else if (all(is.vector(x), is.character(x))) {
    substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    x
  } else {
    stop("argument must be a data frame or character vector")
  }

}

# First to upper ----------------------------------------------------------

#' Convert strings first letter to uppercase
#'
#' This function converts your data frame column names or character vector elements
#' first letter to uppercase. It can be used in a pipe.
#'
#' @param x A data frame or vector
#' @examples
#' biomass %<>%
#'   first_to_upper()
#'
#' first_to_upper(c("first", "second"))
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
#' This function capps the lowest and highest values of a numeric vector at specified percentiles.
#' It can be used both with numeric vectors or data frames with mutate or map_at.
#'
#' @param x A vector or data frame
#' @param floor Bottom percentile. Defaults to 0.025
#' @param roof Top percentile. Defaults to 0.975
#' @examples
#' x <- seq(1, 100, 1)
#' cap_at_percentile(x)
#'
#' data <- data_frame(x = seq(1, 100, 1))
#' data %<>%
#'   mutate(y = cap_at_percentile(x))
#'
#' @export

cap_at_percentile <- function(x, floor = 0.025, roof = 0.975) {

  if (any(!is.numeric(x), !is.vector(x)))
    stop("argument must be a numeric vector")

  object_class <- class(x)

  floor_cap <- as(quantile(x, floor, na.rm = TRUE), object_class)
  roof_cap  <- as(quantile(x, roof, na.rm = TRUE), object_class)

  y <- case_when(
    x > roof_cap ~ roof_cap,
    x < floor_cap ~ floor_cap,
    TRUE ~ x)

  attributes(y) <- NULL
  y

}

# Cap between -------------------------------------------------------------

#' Cap numeric values between two values
#'
#' This function capps the lowest and highest values of a numeric vector between specified values.
#' It can be used both with numeric vectors or data frames with mutate or map_at. If no floor and
#' roof values are provided the function will return exactly the same result as input values.
#'
#' @param x A vector or data frame column
#' @param floor A bottom number. Defaults to NA
#' @param roof A top number. Defaults to NA
#' @examples
#' x <- seq(1, 100, 1)
#' cap_between(x, 40, 60)
#'
#' data <- data_frame(x = seq(1, 100, 1))
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

  y <- case_when(
    x > roof_cap ~ roof_cap,
    x < floor_cap ~ floor_cap,
    TRUE ~ x
  )

}

# Round to ----------------------------------------------------------------

#' Round values to integers
#'
#' This function rounds values of a numeric vector to a selected integer, for example: 1,000 or 10,000.
#' This allows for a more general interpretation of high values.
#'
#' @param x A vector or data frame column
#' @param to An integer to round to. Defaults to a 1,000
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

#' Format a knitr table nicely
#'
#' This function creates nicelycor formatted tables in R Markdown documents. It is
#' designed to work with data formatting functions from the "formattable"
#' package. Remember that columns formating must be applied before calling the
#' format_my_table().
#'
#' @param df A data frame
#' @param format Select the kable format. Possible options are: NA (default), "html" and "latex"
#' @param width Should the table have full-page width? Defaults to FALSE
#' @param font_size What font size should be used? Defaults to 12
#' @param scroll_box Should the table be enframed in a scroll-box? Defaults to
#'   NA. This option is very usefull when dealing with long tables. Must be used
#'   as character in the following format "600px"
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_share(job) %>%
#'   format_my_table()
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_share(job) %>%
#'   mutate(
#'      share   = formattable::percent(share, 2),
#'      n_group = formattable::color_tile("white", "orange")(n_group)
#'   ) %>%
#'   format_my_table()
#' @export
format_my_table <- function(df,
                            format = NA,
                            width = FALSE,
                            font_size = 12,
                            scroll_box = NA) {

  if (any(is.na(format), format == "html")) {

    outcome <- df %>%
      knitr::kable(
        format = "html",
        digits = 3,
        align = "c",
        escape = FALSE  # escape = FALSE enables using the "formattable" package
      ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = width,
        position = "center",
        font_size = font_size
      )

    if (!is.na(scroll_box)) {

      if (!is.character(scroll_box))
        stop("argument must be character")

      outcome %<>%
        kableExtra::scroll_box(height = scroll_box)
    }

  } else if (format == "latex") {

    outcome <- df %>%
      knitr::kable(
        format = "latex",
        digits = 3,
        align = "c",
        booktabs = TRUE
      ) %>%
      kableExtra::kable_styling(
        position = "center",
        latex_options = "scale_down"
      )

  }

  return(outcome)

}

# Change names ------------------------------------------------------------

#' Change a vector or data frame names
#'
#' This function changes your data frame column names or character vector elements according with a specified pattern.
#' By default it fixes column names that have a "." used as words separator to "_".
#'
#' @param x A data frame or vector
#' @param from Character to replace. Defaults to "."
#' @param to Character to apply Defaults to "_"
#' @examples
#' x <- c("Example.1", "Example.2", "Example/3")
#' change_names(x)
#'
#' df <- data_frame(
#'   "Example.1" = c(1, 2),
#'   "Example.2" = c(1, 2),
#'   "Example!3" = c(1, 2))
#'
#' df %>%
#'   change_names()
#' @export
change_names <- function(x, from = ".", to = "_") {

  if (any(!is.character(from), !is.character(to)))
    stop("argument must be character")

  if (is.data.frame(x)) {
    x %>%
      purrr::set_names(~stringr::str_replace_all(.x, glue("\\{from}"), to))
  } else if (all(is.vector(x), is.character(x))) {
    stringr::str_replace_all(x, glue("\\{from}"), to)
  } else {
    stop("argument must be a data frame or character vector")
  }

}
