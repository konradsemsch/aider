
# Calculate bad rate ------------------------------------------------------

#' Calculate bad rate
#'
#' This function calculates bad rates for any number of grouping variables.
#'
#' @param df A data drame
#' @param performance Performance variable
#' @param top_level Top level of the performance variable. Defaults to 1
#' @keywords risk
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   calculate_bad_rate(target)
#'
#' aider_data_v1 %>%
#'   mutate(target = ifelse(target == 1, "Npl", "Pl")) %>%
#'   calculate_bad_rate(target, "Npl")
#'
#' aider_data_v1 %>%
#'   calculate_bad_rate(performance = target, top_level = "1", country)
#' @export
calculate_bad_rate <- function(df,
                               performance,
                               top_level = "1") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  var_performance <- enquo(performance)

  outcome <- df %>%
    mutate(performance_chr = as.character(!!var_performance)) %>%
    summarise(
      n_total = nrow(.),
      n_group = n(),
      bad     = sum(performance_chr == top_level),
      good    = sum(performance_chr != top_level),
      br      = round(bad / n_group, 3)
    ) %>%
    select(
      everything(),
      br,
      bad,
      good,
      n_group,
      n_total
      )

  if (sum(outcome$br) == 0)
    warning("did you set the right performance variable?")

  outcome
}

# Calculate loss rate ------------------------------------------------------

#' Calcualte loss rate
#'
#' This function calculates loss rates for any number of grouping variables.
#'
#' @param df A a data frame
#' @param performance Performance variable
#' @param origination Loans originated variable
#' @param outstanding Loans outstanding variable
#' @param top_level Top level of the performance variable. Defaults to 1
#' @keywords risk
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#' calculate_loss_rate(performance = target,
#'                     origination = originated,
#'                     outstanding = outstanding,
#'                     top_level = "1")
#'
#' aider_data_v1 %>%
#'   mutate(target = ifelse(target == 1, "Npl", "Pl")) %>%
#'   calculate_loss_rate(performance = target,
#'                      origination = originated,
#'                      outstanding = outstanding,
#'                      top_level = "Npl",
#'                      country)
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

#' Calcualte chare
#'
#' This function calculates the share of grand total for any number of grouping variables.
#'
#' @param df A a data frame
#' @param grouping Variable to calculate the share for
#' @keywords risk
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   calculate_share(country)
#'
#' aider_data_v1 %>%
#'   nest(-country) %>%
#'   mutate(stats = map(data, calculate_share, industry)) %>%
#'   select(country, stats) %>%
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
    )
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
#' @keywords risk
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#' calculate_decile_table(binning = sc_v3,
#'                       grouping = target)
#'
#' aider_data_v1 %>%
#'   select(country, target, sc_v3) %>%
#'   nest(-country) %>%
#'   mutate(stats = map(data, calculate_decile_table, sc_v3, target)) %>%
#'   select(country, stats) %>%
#'   unnest()
#' @export
calculate_decile_table <- function(df,
                                   binning,
                                   grouping,
                                   top_level = "1",
                                   n_bins = 10) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(top_level))
    stop("argument must be character")

  if (!is.numeric(n_bins))
    stop("argument must be numeric")

  var_binning  <- enquo(binning)
  var_grouping <- enquo(grouping)

  params <- list(na.rm = T)

  df %>%
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
      ratio        = round(top_level / total, 3)
    ) %>%
    ungroup() %>%
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
}

# Calculate summary statistics, numerical ---------------------------------

#' Calculate statistics
#'
#' This function calculates statistics for numerical attributes for any number of groupong variables.
#'
#' @param df A data frame
#' @keywords risk
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   nest(-country) %>%
#'   mutate(stats = map(data, ~calculate_stats_numeric(.x))) %>%
#'   select(country, stats) %>%
#'   unnest()
#' @export
calculate_stats_numeric <- function(df) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  params <- list(na.rm = T)

  df %>%
    select_if(is.numeric) %>%
    gather("variable", "value") %>%
    drop_na(variable) %>%
    group_by(variable) %>%
    summarize(
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
#' This is a function that enables calculating variable importance for numerical attributes.
#'
#' @param df A a data frame
#' @param target Target variable
#' @param type Type of modelling task. Defaults to classification
#' @param cutoff Include only the most predictive variables. Defaults to 10
#' @keywords risk
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   calculate_importance(target)
#'
#' aider_data_v1 %>%
#'   calculate_importance(target, type = "regression")
#' @export
calculate_importance <- function(df,
                                 target,
                                 type = "classification",
                                 cutoff = 10) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(type))
    stop("argument must be character")

  if (!is.numeric(cutoff))
    stop("argument must be numeric")

  var_target <- enquo(target)

  explanatory <- df %>%
    select_if(is.numeric) %>%
    select(everything(), -!!var_target)

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

  imp <- caret::filterVarImp(x = combined[, -1],
                             y = combined[, 1]$target)

  outcome <- data_frame(
    variable = rownames(imp),
    imp = imp[, 1]
  ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_rank = row_number()) %>%
  filter(imp_rank <= cutoff)
}

# Calculate correlation ----------------------------------------------------

#' Calculate tidy correlation
#'
#' This function calculates correlation among all numerical attributes in a tidy format. Only
#'
#' @param df A a data frame
#' @param cutoff Include correlation higher then a threshold. Defaults to 0.50
#' @param method Which correlation should be computed. Defaults to pearson
#' @param use Which method for computing correlation in presence of missing values. Defaults to pairwise.complete.obs
#' @keywords risk
#' @examples
#' data("aider_data_v1")
#'
#' calculate_correlation(aider_data_v1)
#' @export
calculate_correlation <- function(df,
                                  cutoff = 0.50,
                                  method = "pearson",
                                  use = "pairwise.complete.obs") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.numeric(cutoff))
    stop("argument must be numeric")

  if (any(!is.character(method), !is.character(use)))
    stop("argument must be character")

  df %>%
    select_if(is.numeric) %>%
    corrr::correlate(., method = method, use = use) %>%
    corrr::stretch(.) %>%
    filter(
      x != y,
      abs(r) >= cutoff
    ) %>%
    rename(var_x = x, var_y = y, cor = r) %>%
    filter(!duplicated(pmin(var_x, var_y), pmax(var_x, var_y))) %>%
    arrange(desc(abs(cor)))
}
