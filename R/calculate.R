
# Calculate bad rate ------------------------------------------------------

#' Calculate bad rate
#'
#' This function calculates bad rates for any number of grouping variables.
#'
#' @param df A data drame
#' @param performance Performance variable
#' @param top_level Top level of the performance variable. Defaults to 1
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' data %>%
#'   calculate_bad_rate(status, top_level = "bad")
#'
#' data %>%
#'   mutate(status = ifelse(status == "bad", "Npl", "Pl")) %>%
#'   calculate_bad_rate(status, "Npl")
#'
#' data %>%
#'   calculate_bad_rate(performance = status, top_level = "bad", marital)
#' @export
calculate_bad_rate <- function(df,
                               performance,
                               top_level = "1",
                               ...) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  var_performance <- enquo(performance)
  var_grouping    <- quos(...)

  outcome <- df %>%
    mutate(performance_chr = as.character(!!var_performance)) %>%
    group_by(!!!var_grouping) %>%
    summarise(
      n_total = nrow(.),
      n_group = n(),
      share   = round(n_group / n_total, 3),
      bad     = sum(performance_chr == top_level),
      good    = sum(performance_chr != top_level),
      br      = bad / n_group
    ) %>%
    select(
      everything(),
      br,
      bad,
      good,
      n_group,
      n_total,
      share
    ) %>%
    arrange(desc(n_group))

  if (sum(outcome$br) == 0)
    warning("did you set the right performance variable?")

  outcome

}

# Calculate loss rate ------------------------------------------------------

#' Calculate loss rate
#'
#' This function calculates loss rates for any number of grouping variables.
#'
#' @param df A data frame
#' @param performance Performance variable
#' @param origination Loans originated variable
#' @param outstanding Loans outstanding variable
#' @param top_level Top level of the performance variable. Defaults to 1
#' @export
calculate_loss_rate <- function(df,
                                performance,
                                origination,
                                outstanding,
                                top_level = "1") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  # I don't know yet how to evaluate their types in a nse way
  # if (any(!is.numeric(!!origination), !is.numeric(!!outstanding)))
  #   stop("argument must be numeric")

  if (!is.character(top_level))
    stop("argument must be character")

  var_performance <- enquo(performance)
  var_origination <- enquo(origination)
  Var_outstanding <- enquo(outstanding)

  params <- list(na.rm = T)

  outcome <- df %>%
    mutate(
      performance_chr  = as.character(!!var_performance),
      outstanding_bad  = ifelse(performance_chr == top_level, !!Var_outstanding, 0),
      outstanding_good = ifelse(performance_chr != top_level, !!Var_outstanding, 0)
    ) %>%
    summarise(
      originated       = sum(!!var_origination, !!!params),
      outstanding_bad  = sum(outstanding_bad, !!!params),
      outstanding_good = sum(outstanding_good, !!!params),
      loss_rate        = round(outstanding_bad / originated, 3)
    ) %>%
    select(
      everything(),
      loss_rate,
      outstanding_bad,
      outstanding_good,
      originated
    )

  if (sum(outcome$loss_rate) == 0)
    warning("did you set the right performance variable?")

  outcome

}

# Calculate share ---------------------------------------------------------

#' Calcuate grand total share
#'
#' This function calculates the share of grand total by a selected, grouping variable.
#' If you want to caculate the share in a group by manner you can use the nest-mutate-map
#' combination. See examples for an illustration.
#'
#' @param df A a data frame
#' @param grouping Variable to calculate the share for
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_share(marital)
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   nest(-marital) %>%
#'   mutate(stats = map(data, calculate_share, job)) %>%
#'   select(marital, stats) %>%
#'   unnest()
#' @export
calculate_share <- function(df,
                            grouping) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_grouping <- enquo(grouping)

  df %>%
    group_by(!!!var_grouping) %>%
    summarise(
      n_total = nrow(.),
      n_group = n(),
      share = round(n_group / n_total, 3)
    ) %>%
    select(
      everything(),
      share,
      n_group,
      n_total
    ) %>%
    arrange(desc(n_group))

}

# Calculate decile table --------------------------------------------------

#' Calculate a decile breakdown
#'
#' This function calculates a decile table for any combination of numerical and categorical variables.
#'
#' @param df A data frame
#' @param binning Variable for which binning should be applied
#' @param grouping A two-level (binary) variable to calculate the ratio in each bin
#' @param top_level Top level of the grouping variable. Defaults to 1
#' @param n_bins Provide a number of bins. Defaults to 10
#' @param risk_names Should column names be converted to risk-specific names? Defaults to TRUE
#' @param format Should table printing be formatted with kable? Defaults to FALSE
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_decile_table(binning = time,
#'                       grouping = status,
#'                       top_level = "bad")
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   select(marital, status, time) %>%
#'   nest(-marital) %>%
#'   mutate(stats = map(data, calculate_decile_table, time, status, "bad")) %>%
#'   select(marital, stats) %>%
#'   unnest()
#' @export
calculate_decile_table <- function(df,
                                   binning,
                                   grouping,
                                   top_level = "1",
                                   n_bins = 10,
                                   risk_names = TRUE,
                                   format = FALSE) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  if (!is.numeric(n_bins))
    stop("argument must be numeric")

  var_binning  <- enquo(binning)
  var_grouping <- enquo(grouping)

  params <- list(na.rm = T)

  outcome <- df %>%
    drop_na(!!var_binning) %>%
    mutate(
      decile = as.factor(ntile(!!var_binning, n_bins)),
      grouping_chr = as.character(!!var_grouping)
    ) %>%
    group_by(decile) %>%
    summarize(
      min          = round(min(!!var_binning, !!!params), 3),
      median       = round(median(!!var_binning, !!!params), 3),
      max          = round(max(!!var_binning, !!!params), 3),
      top_level    = sum(grouping_chr == top_level),
      bottom_level = sum(grouping_chr != top_level),
      total        = n(),
      ratio        = top_level / total
    ) %>%
    ungroup() %>%
    mutate_at(vars(one_of(c("min", "median", "max"))), round, 2) %>%
    select(
      decile,
      min,
      median,
      max,
      top_level,
      bottom_level,
      total,
      ratio
    )

  if (risk_names == TRUE) {
    outcome %<>%
      rename(
        npl = top_level,
        pl  = bottom_level,
        badRate = ratio
      )
  }

  var_format <- c("ratio", "badRate")

  if (format == TRUE) {
    outcome %<>%
      mutate_at(vars(one_of(var_format)), ~formattable::color_tile("white", "red")(.x)) %>%
      first_to_upper() %>%
      format_my_table()
  }

  return(outcome)

}

# Calculate summary statistics, numerical ---------------------------------

#' Calculate statistics of numericall attributes
#'
#' This function calculates statistics for numerical attributes. All numerical variables
#' are selected by the function automatically. If you want to apply it so specific columns only
#' you need to specify that prior or after the function is applied. If you want to caculate
#' the share in a group by manner you can use the nest-mutate-map combination.
#' See examples for an illustration.
#'
#' @param df A data frame
#' @examples
#' credit_data %>%
#'   calculate_stats_numeric()
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   nest(-marital) %>%
#'   mutate(stats = map(data, ~calculate_stats_numeric(.x))) %>%
#'   select(marital, stats) %>%
#'   unnest()
#' @export
calculate_stats_numeric <- function(df) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  column_types <- df %>%
    map(class) %>%
    unlist(use.names = FALSE) %>%
    keep(~ .x %in% c("numeric", "integer"))

  if (length(column_types) == 0)
    stop("data frame must have at least one numeric variable")

  params <- list(na.rm = T)

  message(glue("Number of observations: {nrow(df)}"))

  df %>%
    select_if(is.numeric) %>%
    gather("variable", "value") %>%
    drop_na(variable) %>%
    group_by(variable) %>%
    summarize(
      na       = sum(is.na(value)),
      min      = min(value, !!!params),
      p5       = quantile(value, .05, !!!params),
      q25      = quantile(value, .25, !!!params),
      q50      = quantile(value, .5, !!!params),
      q75      = quantile(value, .75, !!!params),
      p95      = quantile(value, .95, !!!params),
      max      = max(value, !!!params),
      avg      = mean(value, !!!params),
      avg_trim = mean(value, trim = .05, !!!params),
      std      = sd(value, !!!params)
    ) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2)

}

# Calculate importance ----------------------------------------------------

#' Find the most important variables
#'
#' This function calculates importance in a tidy way by extending the filterVarImp() function from caret.
#'
#' @param df A a data frame
#' @param target Target variable
#' @param type Type of modelling task. Defaults to classification
#' @examples
#' credit_data %>%
#'   calculate_importance(Status)
#'
#' credit_data %>%
#'   calculate_importance(Status, type = "regression")
#' @export
calculate_importance <- function(df,
                                 target,
                                 type = "classification") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(type))
    stop("argument must be character")

  var_target <- enquo(target)

  explanatory <- df %>%
    select(-!!var_target)

  reponse <- df %>%
    select(target = !!var_target)

  if (type == "classification") {
    reponse$target <- as.factor(reponse$target)
  } else {
    reponse
  }

  combined <- bind_cols(
    reponse,
    explanatory
  )

  imp <- filterVarImp(x = combined[, -1],
                      y = combined$target)

  outcome <- data_frame(
    variable = rownames(imp),
    imp = imp[, 1]
    ) %>%
    arrange(desc(imp)) %>%
    mutate(
      imp_norm = (imp - min(imp)) / (max(imp) - min(imp)),
      imp_norm = formattable::percent(imp_norm),
      imp_rank = row_number()
      )

}

# Calculate correlation ----------------------------------------------------

#' Calculate tidy correlation
#'
#' This function calculates correlation among all numerical attributes in a tidy format.
#'
#' @param df A a data frame
#' @param cutoff Include correlation higher then a threshold. Defaults to 0 - all variables are included
#' @param method Which correlation should be computed. Defaults to pearson
#' @param use Which method for computing correlation in presence of missing values. Defaults to pairwise.complete.obs
#' @param dedup Should all rows of the resulting table be deduplicated? Defaults to TRUE
#' @examples
#' calculate_correlation(credit_data)
#' @export
calculate_correlation <- function(df,
                                  cutoff = 0,
                                  method = "pearson",
                                  use = "pairwise.complete.obs",
                                  dedup = TRUE
                                  ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.numeric(cutoff))
    stop("argument must be numeric")

  if (any(!is.character(method), !is.character(use)))
    stop("argument must be character")

  df %<>%
    select_if(is.numeric) %>%
    corrr::correlate(., method = method, use = use) %>%
    corrr::stretch(.) %>%
    filter(
      x != y,
      abs(r) >= cutoff
    ) %>%
    rename(var_x = x, var_y = y, cor = r) %>%
    mutate(
      x_dedup = pmin(var_x, var_y),
      y_dedup = pmax(var_x, var_y)
    ) %>%
    arrange(desc(abs(cor)))

  if (dedup == TRUE){
  df %<>%
    distinct(x_dedup, y_dedup, .keep_all = TRUE) %>%
    select(-x_dedup, -y_dedup)
  return(df)
  } else {
  return(df)
  }

}

# Count unique ------------------------------------------------------------

#' Count unique observations
#'
#' This function calculates the number of unique observations in a vector.
#'
#' @examples
#' count_proportions(credit_data$Marital)
#' @export
count_unique <- compose(length, unique)

# Count proportions -------------------------------------------------------

#' Count proportions of levels
#'
#' This function calculates the proportions of unique observations in a vector.
#'
#' @param x A vector
#' @examples
#' count_proportions(credit_data$Marital)
#' @export
count_proportions <- function(x) {

  outcome <- prop.table(table(x)) %>%
    map_dbl(round, 2)

  formattable::percent(outcome)

}

# Calculate bad rate ------------------------------------------------------

#' Calculate bad rate
#'
#' This function calculates bad rates for any number of grouping variables.
#'
#' @param df A data drame
#' @param performance Performance variable
#' @param top_level Top level of the performance variable. Defaults to 1
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' data %>%
#'   calculate_bad_rate(status, top_level = "bad")
#'
#' data %>%
#'   mutate(status = ifelse(status == "bad", "Npl", "Pl")) %>%
#'   calculate_bad_rate(status, "Npl")
#'
#' data %>%
#'   calculate_bad_rate(performance = status, top_level = "bad", marital)
#' @export
calculate_bad_rate <- function(df,
                               performance,
                               top_level = "1",
                               ...) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  var_performance <- enquo(performance)
  var_grouping    <- quos(...)

  outcome <- df %>%
    mutate(performance_chr = as.character(!!var_performance)) %>%
    group_by(!!!var_grouping) %>%
    summarise(
      n_total = nrow(.),
      n_group = n(),
      share   = round(n_group / n_total, 3),
      bad     = sum(performance_chr == top_level),
      good    = sum(performance_chr != top_level),
      br      = bad / n_group
    ) %>%
    select(
      everything(),
      br,
      bad,
      good,
      n_group,
      n_total,
      share
    ) %>%
    arrange(desc(n_group))

  if (sum(outcome$br) == 0)
    warning("did you set the right performance variable?")

  outcome

}

# Calculate loss rate ------------------------------------------------------

#' Calculate loss rate
#'
#' This function calculates loss rates for any number of grouping variables.
#'
#' @param df A data frame
#' @param performance Performance variable
#' @param origination Loans originated variable
#' @param outstanding Loans outstanding variable
#' @param top_level Top level of the performance variable. Defaults to 1
#' @export
calculate_loss_rate <- function(df,
                                performance,
                                origination,
                                outstanding,
                                top_level = "1") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  # I don't know yet how to evaluate their types in a nse way
  # if (any(!is.numeric(!!origination), !is.numeric(!!outstanding)))
  #   stop("argument must be numeric")

  if (!is.character(top_level))
    stop("argument must be character")

  var_performance <- enquo(performance)
  var_origination <- enquo(origination)
  Var_outstanding <- enquo(outstanding)

  params <- list(na.rm = T)

  outcome <- df %>%
    mutate(
      performance_chr  = as.character(!!var_performance),
      outstanding_bad  = ifelse(performance_chr == top_level, !!Var_outstanding, 0),
      outstanding_good = ifelse(performance_chr != top_level, !!Var_outstanding, 0)
    ) %>%
    summarise(
      originated       = sum(!!var_origination, !!!params),
      outstanding_bad  = sum(outstanding_bad, !!!params),
      outstanding_good = sum(outstanding_good, !!!params),
      loss_rate        = round(outstanding_bad / originated, 3)
    ) %>%
    select(
      everything(),
      loss_rate,
      outstanding_bad,
      outstanding_good,
      originated
    )

  if (sum(outcome$loss_rate) == 0)
    warning("did you set the right performance variable?")

  outcome

}

# Calculate share ---------------------------------------------------------

#' Calcuate grand total share
#'
#' This function calculates the share of grand total by a selected, grouping variable.
#' If you want to caculate the share in a group by manner you can use the nest-mutate-map
#' combination. See examples for an illustration.
#'
#' @param df A a data frame
#' @param grouping Variable to calculate the share for
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_share(marital)
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   nest(-marital) %>%
#'   mutate(stats = map(data, calculate_share, job)) %>%
#'   select(marital, stats) %>%
#'   unnest()
#' @export
calculate_share <- function(df,
                            grouping) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_grouping <- enquo(grouping)

  df %>%
    group_by(!!!var_grouping) %>%
    summarise(
      n_total = nrow(.),
      n_group = n(),
      share = round(n_group / n_total, 3)
    ) %>%
    select(
      everything(),
      share,
      n_group,
      n_total
    ) %>%
    arrange(desc(n_group))

}

# Calculate decile table --------------------------------------------------

#' Calculate a decile breakdown
#'
#' This function calculates a decile table for any combination of numerical and categorical variables.
#'
#' @param df A data frame
#' @param binning Variable for which binning should be applied
#' @param grouping A two-level (binary) variable to calculate the ratio in each bin
#' @param top_level Top level of the grouping variable. Defaults to 1
#' @param n_bins Provide a number of bins. Defaults to 10
#' @param risk_names Should column names be converted to risk-specific names? Defaults to TRUE
#' @param format Should table printing be formatted with kable? Defaults to FALSE
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_decile_table(binning = time,
#'                       grouping = status,
#'                       top_level = "bad")
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   select(marital, status, time) %>%
#'   nest(-marital) %>%
#'   mutate(stats = map(data, calculate_decile_table, time, status, "bad")) %>%
#'   select(marital, stats) %>%
#'   unnest()
#' @export
calculate_decile_table <- function(df,
                                   binning,
                                   grouping,
                                   top_level = "1",
                                   n_bins = 10,
                                   risk_names = TRUE,
                                   format = FALSE) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  if (!is.numeric(n_bins))
    stop("argument must be numeric")

  var_binning  <- enquo(binning)
  var_grouping <- enquo(grouping)

  params <- list(na.rm = T)

  outcome <- df %>%
    drop_na(!!var_binning) %>%
    mutate(
      decile = as.factor(ntile(!!var_binning, n_bins)),
      grouping_chr = as.character(!!var_grouping)
    ) %>%
    group_by(decile) %>%
    summarize(
      min          = round(min(!!var_binning, !!!params), 3),
      median       = round(median(!!var_binning, !!!params), 3),
      max          = round(max(!!var_binning, !!!params), 3),
      top_level    = sum(grouping_chr == top_level),
      bottom_level = sum(grouping_chr != top_level),
      total        = n(),
      ratio        = top_level / total
    ) %>%
    ungroup() %>%
    mutate_at(vars(one_of(c("min", "median", "max"))), round, 2) %>%
    select(
      decile,
      min,
      median,
      max,
      top_level,
      bottom_level,
      total,
      ratio
    )

  if (risk_names == TRUE) {
    outcome %<>%
      rename(
        npl = top_level,
        pl  = bottom_level,
        badRate = ratio
      )
  }

  var_format <- c("ratio", "badRate")

  if (format == TRUE) {
    outcome %<>%
      mutate_at(vars(one_of(var_format)), ~formattable::color_tile("white", "red")(.x)) %>%
      first_to_upper() %>%
      format_my_table()
  }

  return(outcome)

}


# Calculate log-odds table ------------------------------------------------

#' Calculate a log-odds table
#'
#' This function calculates a log-odds table for any combination of numerical and categorical variables.
#'
#' @param df A data frame
#' @param binning Variable for which binning should be applied
#' @param grouping A two-level (binary) variable to calculate the ratio in each bin
#' @param top_level Top level of the grouping variable. Defaults to 1
#' @param n_bins Provide a number of bins. Defaults to 10
#' @param risk_names Should column names be converted to risk-specific names? Defaults to TRUE
#' @param format Should table printing be formatted with kable? Defaults to FALSE
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_logodds_table(binning = time,
#'                           grouping = status,
#'                           top_level = "bad")
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   select(marital, status, time) %>%
#'   nest(-marital) %>%
#'   mutate(stats = map(data, calculate_logodds_table, time, status, "bad")) %>%
#'   select(marital, stats) %>%
#'   unnest()
#' @export
calculate_logodds_table <- function(df,
                                    binning,
                                    grouping,
                                    top_level = "1",
                                    n_bins = 10,
                                    risk_names = TRUE,
                                    format = FALSE) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  if (!is.numeric(n_bins))
    stop("argument must be numeric")

  var_binning  <- enquo(binning)
  var_grouping <- enquo(grouping)

  params <- list(na.rm = T)

  outcome <- df %>%
    drop_na(!!var_binning) %>%
    mutate(
      decile = as.factor(ntile(!!var_binning, n_bins)),
      grouping_chr = as.character(!!var_grouping)
    ) %>%
    group_by(decile) %>%
    summarize(
      mean         = round(mean(!!var_binning, !!!params), 3),
      top_level    = sum(grouping_chr == top_level),
      bottom_level = sum(grouping_chr != top_level),
      total        = n(),
      prob         = top_level / total,
      odds         = prob / (1 - prob),
      log_odds     = log(odds)
    ) %>%
    ungroup() %>%
    mutate_at(vars(one_of(c("mean"))), round, 2) %>%
    select(
      decile,
      mean,
      top_level,
      bottom_level,
      total,
      prob,
      odds,
      log_odds
    )

  if (risk_names == TRUE) {
    outcome %<>%
      rename(
        npl = top_level,
        pl  = bottom_level
      )
  }

  if (format == TRUE) {
    outcome %<>%
      first_to_upper() %>%
      format_my_table()
  }

  return(outcome)

}

# Calculate summary statistics, numerical ---------------------------------

#' Calculate statistics of numericall attributes
#'
#' This function calculates statistics for numerical attributes. All numerical variables
#' are selected by the function automatically. If you want to apply it so specific columns only
#' you need to specify that prior or after the function is applied. If you want to caculate
#' the share in a group by manner you can use the nest-mutate-map combination.
#' See examples for an illustration.
#'
#' @param df A data frame
#' @examples
#' credit_data %>%
#'   calculate_stats_numeric()
#'
#' credit_data %>%
#'   first_to_lower() %>%
#'   nest(-marital) %>%
#'   mutate(stats = map(data, ~calculate_stats_numeric(.x))) %>%
#'   select(marital, stats) %>%
#'   unnest()
#' @export
calculate_stats_numeric <- function(df) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  column_types <- df %>%
    map(class) %>%
    unlist(use.names = FALSE) %>%
    keep(~ .x %in% c("numeric", "integer"))

  if (length(column_types) == 0)
    stop("data frame must have at least one numeric variable")

  params <- list(na.rm = T)

  message(glue("Number of observations: {nrow(df)}"))

  df %>%
    select_if(is.numeric) %>%
    gather("variable", "value") %>%
    drop_na(variable) %>%
    group_by(variable) %>%
    summarize(
      na       = sum(is.na(value)),
      min      = min(value, !!!params),
      p5       = quantile(value, .05, !!!params),
      q25      = quantile(value, .25, !!!params),
      q50      = quantile(value, .5, !!!params),
      q75      = quantile(value, .75, !!!params),
      p95      = quantile(value, .95, !!!params),
      max      = max(value, !!!params),
      avg      = mean(value, !!!params),
      avg_trim = mean(value, trim = .05, !!!params),
      std      = sd(value, !!!params)
    ) %>%
    ungroup() %>%
    mutate_if(is.numeric, round, 2)

}

# Calculate importance ----------------------------------------------------

#' Find the most important variables
#'
#' This function calculates importance in a tidy way by extending the filterVarImp() function from caret.
#'
#' @param df A a data frame
#' @param target Target variable
#' @param type Type of modelling task. Defaults to classification
#' @examples
#' credit_data %>%
#'   calculate_importance(Status)
#'
#' credit_data %>%
#'   calculate_importance(Status, type = "regression")
#' @export
calculate_importance <- function(df,
                                 target,
                                 type = "classification") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(type))
    stop("argument must be character")

  var_target <- enquo(target)

  explanatory <- df %>%
    select(-!!var_target)

  reponse <- df %>%
    select(target = !!var_target)

  if (type == "classification") {
    reponse$target <- as.factor(reponse$target)
  } else {
    reponse
  }

  combined <- bind_cols(
    reponse,
    explanatory
  )

  imp <- filterVarImp(x = combined[, -1],
                      y = combined$target)

  outcome <- data_frame(
    variable = rownames(imp),
    imp = imp[, 1]
    ) %>%
    arrange(desc(imp)) %>%
    mutate(
      imp_norm = (imp - min(imp)) / (max(imp) - min(imp)),
      imp_norm = formattable::percent(imp_norm),
      imp_rank = row_number()
      )

}

# Calculate correlation ----------------------------------------------------

#' Calculate tidy correlation
#'
#' This function calculates correlation among all numerical attributes in a tidy format.
#'
#' @param df A a data frame
#' @param cutoff Include correlation higher then a threshold. Defaults to 0 - all variables are included
#' @param method Which correlation should be computed. Defaults to spearman
#' @param use Which method for computing correlation in presence of missing values. Defaults to pairwise.complete.obs
#' @param dedup Should all rows of the resulting table be deduplicated? Defaults to TRUE
#' @examples
#' calculate_correlation(credit_data)
#' @export
calculate_correlation <- function(df,
                                  cutoff = 0,
                                  method = "spearman",
                                  use = "pairwise.complete.obs",
                                  dedup = TRUE
                                  ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.numeric(cutoff))
    stop("argument must be numeric")

  if (any(!is.character(method), !is.character(use)))
    stop("argument must be character")

  df %<>%
    select_if(is.numeric) %>%
    corrr::correlate(., method = method, use = use) %>%
    corrr::stretch(.) %>%
    filter(
      x != y,
      abs(r) >= cutoff
    ) %>%
    rename(var_x = x, var_y = y, cor = r) %>%
    mutate(
      x_dedup = pmin(var_x, var_y),
      y_dedup = pmax(var_x, var_y)
    ) %>%
    arrange(desc(abs(cor)))

  if (dedup == TRUE){
  df %<>%
    distinct(x_dedup, y_dedup, .keep_all = TRUE) %>%
    select(-x_dedup, -y_dedup)
  return(df)
  } else {
  return(df)
  }

}

# Count unique ------------------------------------------------------------

#' Count unique observations
#'
#' This function calculates the number of unique observations in a vector.
#'
#' @examples
#' count_proportions(credit_data$Marital)
#' @export
count_unique <- compose(length, unique)

# Count proportions -------------------------------------------------------

#' Count proportions of levels
#'
#' This function calculates the proportions of unique observations in a vector.
#'
#' @param x A vector
#' @examples
#' count_proportions(credit_data$Marital)
#' @export
count_proportions <- function(x) {

  outcome <- prop.table(table(x)) %>%
    map_dbl(round, 2)

  formattable::percent(outcome)

}
