
# Apply a model recipe ----------------------------------------------------

#' Apply a model recipe
#'
#' This function calculates a simple and universal model matrix recipe that can be used
#' for a number of standardised modelling tasks.
#'
#' @param df A data frame
#' @param target A target variable
#' @examples
#' apply_recipe(credit_data, Status)
#' @export
apply_recipe <- function(df, target) {

  var_target <- enquo(target)

  var_predictors <- df %>%
    select(-!!var_target) %>%
    names()

  var_numeric <- df %>%
    select(-!!var_target) %>%
    select_if(is.numeric) %>%
    names()

  var_types <- df %>%
    select(-!!var_target) %>%
    map(class) %>%
    map_df(1) %>%
    gather() %>%
    group_by(value) %>%
    count()

  recipe <- recipe(df) %>%
    add_role(!!var_target, new_role = "outcome") %>%
    add_role(one_of(var_predictors), new_role = "predictor")

  # Converting dates if available
  if (any(var_types$value %in% c("POSIXct"))) {
    recipe %<>%
      step_date(has_type(match = "date")) %>%
      step_rm(has_type(match = "date"))
  }

  # Imputting and one-hot encoding of nominal variables if available
  if (any(var_types$value %in% c("factor", "character"))) {
    recipe %<>%
      step_modeimpute(all_nominal(), -!!var_target) %>%
      step_dummy(all_nominal(), -!!var_target)
  }

  recipe %<>%
    step_meanimpute(all_numeric()) %>%
    step_center(one_of(var_numeric)) %>%
    step_scale(one_of(var_numeric))

}

# Analyse missing values --------------------------------------------------

#' Analyse missing values
#'
#' This function returns a set of basic summary statistics of missing values in a data frame
#'
#' @param df A data frame
#' @param case_cutoff A cut-off to narrow down cases with higher percentage of missing values
#' @param var_cutoff A cut-off to narrow down variables with higher percentage of missing values
#' @examples
#' analyse_missing(credit_data)
#' @export

analyse_missing <- function(df,
                            case_cutoff = 30.0,
                            var_cutoff = 20.0
                            ){

  # General missing stats
  stats_missing_case <- naniar::miss_case_summary(df) %>%
    mutate(pct_miss = round(pct_miss, 2))

  stats_missing_var  <- naniar::miss_var_summary(df) %>%
    mutate(pct_miss = round(pct_miss, 2))

  plot_missing_case <- naniar::gg_miss_case(df, show_pct = TRUE)
  plot_missing_var  <- naniar::gg_miss_var(df, show_pct = TRUE)

  # Overview of missing patterns
  patterns_missing <- naniar::gg_miss_upset(df)

  # Particular cases deepdive
  df_missing_prop <- df %>%
    naniar::add_prop_miss() %>%
    mutate(
      row_number = row_number(),
      prop_miss_all = round(prop_miss_all, 2)
      ) %>%
    arrange(desc(prop_miss_all)) %>%
    select(row_number, prop_miss_all, everything())

  # Top missing stats
  stats_top_missing_case <- df_missing_prop %>%
    filter(prop_miss_all >= case_cutoff / 100)

  stats_top_missing_case <- if (nrow(stats_top_missing_case) == 0) {
    "No cases excluded"
  }

  stats_top_missing_var <- stats_missing_var %>%
    filter(pct_miss >= var_cutoff) %>%
    .$variable

  stats_top_missing_var <- if (length(stats_top_missing_var) == 0) {
    "No variables excluded"
  }

  output <- list(
    stats_missing_by_case = stats_missing_case,
    stats_missing_by_var = stats_missing_var,
    plot_missing_by_case = plot_missing_case,
    plot_missing_by_var = plot_missing_var,
    stats_top_missing_case = stats_top_missing_case,
    stats_top_missing_var = stats_top_missing_var,
    df_missing_prop = df_missing_prop
  )

}

# Apply multivariate outlier detection ------------------------------------

#' Apply multivariate outlier detection
#'
#' This function calculates individual observations outlier score with the Lof algorithm and
#' returns the original data frame with the Lof outlier score, Lof stats and variables with the
#' highest impact on the Lof score, as well as a data frame with the most outlying cases (approx. top 5%).
#'
#' @param df A data frame
#' @examples
#' data <- recipes::credit_data %>%
#'   apply_recipe(Status) %>%
#'   prep(retain = TRUE) %>%
#'   juice() %>%
#'   select(-Status)
#'
#' out <- apply_mov(data)
#' @export
apply_mov <- function(df) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  out_scores <- Rlof::lof(df, c(5:10)) %>%
    as_data_frame() %>%
    rowwise() %>%
    mutate(
      lof = round(median(c(`5`, `6`, `7`, `8`, `9`, `10`)), 2)
    ) %>%
    select(lof)

  df %<>%
    add_column(lof = out_scores$lof, .before = 1)

  lof_stats <- calculate_stats_numeric(df["lof"])
  lof_imp   <- calculate_importance(df, lof, "regression")

  df_out <- df %>%
    mutate(row_number = row_number()) %>%
    filter(lof >= lof_stats$avg + lof_stats$std * 3) %>%
    arrange(desc(lof)) %>%
    select(row_number, lof, everything())

  outcome <- list(
    df = df,
    lof_stats = lof_stats,
    lof_imp = lof_imp,
    df_out = df_out
  )

}

# Analyse variables predictiveness ----------------------------------------

#' Analyse variables predictiveness
#'
#' This function calculates importance of all variables and selects the ones that are
#' the most predictive and uncorrelated to each other based on a selected correlation cutoff.
#' This process is performed iteratively from the most to least important variables. Only numerical
#' attributes are considered.
#'
#' @param df A a data frame
#' @param target Target variable
#' @param cutoff Exclude variables that have a correlation higher then a specified threshold
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' analyse_predictiveness(data, status, 0.5)
#' @export
analyse_predictiveness <- function(df,
                                   target,
                                   cutoff
                                   ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.numeric(cutoff))
    stop("argument must be numeric")

  var_target <- enquo(target)

  data_cor <- df %>%
    calculate_correlation(dedup = FALSE)

  data_imp <- df %>%
    calculate_importance(!!var_target) %>%
    filter(variable %in% unique(data_cor$var_x))

  var_analysed <- vector("list", length = nrow(data_imp))
  var_analysed[[1]] <- data_imp$variable[[1]]

  var_selected <- data_frame(
    variable = c(NA),
    decision = c(NA),
    cor_max  = c(NA),
    cor_with = c(NA)
  )

  var_selected[1, ]$variable <- data_imp$variable[[1]]
  var_selected[1, ]$decision <- "include"
  var_selected[1, ]$cor_max  <- 0
  var_selected[1, ]$cor_with <- "self"

  for (var in seq_along(data_imp$variable)) {

    var_name <- data_imp$variable[[var]]
    var_imp  <- data_imp$imp[[var]]
    var_rank <- data_imp$imp_rank[[var]]

    message(glue("Var rank {var_rank}: {var_name}, {round(var_imp, 2)} importance"))

    if (var_name %in% var_analysed) {

      next

    } else {

      cor_check <- data_cor %>%
        filter(
          var_x == var_name,
          var_y %in% var_analysed
        ) %>%
        arrange(desc(abs(cor))) %>%
        slice(1)

      if (abs(cor_check$cor) >= cutoff) {

        var_selected[var, ]$variable <- var_name
        var_selected[var, ]$decision <- "exclude"
        var_selected[var, ]$cor_max  <- cor_check$cor
        var_selected[var, ]$cor_with <- cor_check$var_y

      } else {

        var_analysed[[var]] <- var_name

        var_selected[var, ]$variable <- var_name
        var_selected[var, ]$decision <- "include"
        var_selected[var, ]$cor_max  <- cor_check$cor
        var_selected[var, ]$cor_with <- cor_check$var_y

      }
    }
  }

  message(glue("{length(unlist(var_analysed))} variables were included in the final set"))

  outcome <- data_imp %>%
    left_join(
      filter(var_selected, !is.na(variable)),
      "variable"
    )

}

# Apply recursive feature elimination -------------------------------------

#' Apply recursive feature elimination
#'
#' This function performes Recursive Feature Elimination (RFE) based on the implementation in the caret package.
#' It is currently only implemented for classification problems. The best subset of variables is selected through
#' maximizing the F1 score. The RFE process is performed by applying bootstrap sampling.
#'
#' @param df A a data frame
#' @param target Target variable
#' @param subsets Provide a vector of the number of variables to fit models to. Defaults to c(4, 8, 16, 24, 32)
#' @param number Number of repeats of the RFE process. Defaults to 25
#' @param ntree Number of random forest trees to fit. Defaults to 500
#' @param downsample Should the majority class be downsampled during resampling? Defaults to "yes"
#' @examples
#' data <- credit_data %>%
#'   first_to_lower() %>%
#'   apply_recipe(status) %>%
#'   prep(retain = TRUE) %>%
#'   juice()
#'
#' rfe <- apply_rfe(data, status)
#' @export
apply_rfe <- function(df,
                      target,
                      subsets = c(4, 8, 16, 24, 32),
                      number = 25,
                      ntree = 500,
                      downsample = "yes"
                      ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (is.null(subsets))
    stop("object can't be NULL")

  var_target <- enquo(target)

  df %<>%
    rename(target = !!var_target)

  new_rf <- rfFuncs

  rf_stats <- function(...) c(
    twoClassSummary(...),
    prSummary(...)
  )

  if (downsample == "yes") {
    rf_fit <- function(x, y, first, last, ...){
      loadNamespace("randomForest")

      df_up <- caret::downSample(x, y)

      randomForest::randomForest(
        select(df_up, -Class),
        df_up$Class,
        importance = TRUE,
        ...)
    }

    new_rf$fit <- rf_fit
  }

  rf_size <- function (x, metric, maximize) {
    best <- caret::pickSizeBest(x, metric = "F", maximize = TRUE)
  }

  new_rf$summary <- rf_stats
  new_rf$selectSize <- rf_size

  rfe_ctrl <- caret::rfeControl(
    method = "boot",
    returnResamp = "final",
    number = number,
    verbose = TRUE,
    saveDetails = TRUE,
    functions = new_rf
  )

  rfe <- caret::rfe(
    x = select(df, -target),
    y = df$target,
    sizes = subsets,
    metric = "AUC",
    rfeControl = rfe_ctrl,
    ntree = ntree
  )

  rfe_imp <- data_frame(
      variable = rownames(varImp(rfe)),
      imp = varImp(rfe)[, 1]
    ) %>%
    arrange(desc(imp)) %>%
    mutate(
      imp_norm = (imp - min(imp)) / (max(imp) - min(imp)),
      imp_norm = formattable::percent(imp_norm),
      imp_rank = row_number(),
      imp      = formattable::digits(imp, 3)
    )

  outcome <- list(
    rfe = rfe,
    rfe_imp = rfe_imp
  )

}

# Analyse interactions ----------------------------------------------------

#' Find meaningfull interactions
#'
#' This function calculates a set of potentially meaningful side-effects. Second level interactions are calculated
#' through estimating and tuning two models with repeated cross-validation: MARS and Lasso regression.
#' Data preprocessing happens automatically through applying the BP2 recipe blueprint.
#'
#' @param df A data frame
#' @param target A target variable
#' @param folds Specify the number of folds in cross-validation. Defaults to 5
#' @param repeats Specify the number of times the fitting process should be repeated. Defaults to 1
#' @param upsample Should the minority class be upsampled during resampling? Defaults to "no"
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' @export
analyse_interactions <- function(df,
                                 target,
                                 folds = 5,
                                 repeats = 1,
                                 upsample = "no"
                                 ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_target <- enquo(target)

  df %<>%
    rename(target = !!var_target)

  splits <- createMultiFolds(df$target, k = folds, times = repeats)

  ctrl <- trainControl(
    method = "repeatedcv",
    repeats = repeats,
    number = folds,
    index = splits,
    classProbs = TRUE,
    verboseIter = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )

  if (upsample == "yes") {
    ctrl$sampling <- "up"
  }

  # MARS model
  # recipe_mars <- apply_recipe_bp3(df, target)
  #
  # grid_mars <- data.frame(
  #   degree = 2,
  #   nprune = seq(10, 60, by = 5)
  # )
  #
  # model_mars <- train(
  #   recipe_mars,
  #   data = df,
  #   method = "earth",
  #   trControl = ctrl,
  #   tuneGrid = grid_mars
  # )

  # Elastic-net model
  recipe_enet <- apply_recipe(df, target) %>%
    step_interact(terms = ~ (. - target)^2)

  grid_enet <- expand.grid(
    alpha = c(0, .25, .50, .75, 1),
    lambda = 10 ^ seq(-4, 0, length = 30)
  )

  model_enet <- train(
    recipe_enet,
    data = df,
    method = "glmnet",
    trControl = ctrl,
    tuneGrid = grid_enet
  )

  # Selecting interactions terms

  # MARS model interactions
  # mars_coef <- summary(model_mars)$glm.coefficients %>%
  #   as.matrix() %>%
  #   as_data_frame()
  #
  # mars_int <- attributes(summary(model_mars)$glm.coefficients)$dimnames[[1]] %>%
  #   as_data_frame() %>%
  #   bind_cols(mars_coef)
  #
  # colnames(mars_int) <- c("value", "coef")

  # Elastic-net model interactions
  enet_coef <- coef(model_enet$finalModel, model_enet$bestTune$lambda) %>%
    as.matrix() %>%
    as_data_frame()

  enet_coef_int <- coef(model_enet$finalModel, model_enet$bestTune$lambda)@Dimnames[[1]] %>%
    as_data_frame() %>%
    bind_cols(enet_coef) %>%
    rename(variable = value, coef = `1`) %>%
    filter(
      coef != 0,
      grepl("_x_", variable)
    )


  output <- list(
    # mars = model_mars,
    enet = model_enet,

    # mars_int = mars_int,
    enet_int_coef = enet_coef_int
  )

}

# Analyse transformations -------------------------------------------------

#' Find predictive variable transformations
#'
#' TBD
#'
#' @param df A data frame
#' @param target A target variable
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' transformations <- analyse_transformations(data, status)
#' @export
analyse_transformations <- function(df,
                                    target
                                    ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_target <- enquo(target)

  vec_numeric <- df %>%
    select_if(is.numeric) %>%
    names(.)

  vec_positive <- df %>%
    select(one_of(vec_numeric)) %>%
    map(~min(.x, na.rm = TRUE)) %>%
    keep(~.x > 0) %>%
    names(.)

  df %<>%
    rename(target = !!var_target) %>%
    mutate(target = as.numeric(target))

  # Sign agnostic transformations
  rec_base <- df %>%
    select(one_of(vec_numeric)) %>%
    add_column(target = df$target) %>%
    recipe(target ~ .) %>%
    step_meanimpute(all_numeric())

  # Sign agnostic transformations
  rec_yj <- df %>%
    select(one_of(vec_numeric)) %>%
    add_column(target = df$target) %>%
    recipe(target ~ .) %>%
    step_meanimpute(all_numeric()) %>%
    step_YeoJohnson(all_numeric())

  rec_logs <- df %>%
    select(one_of(vec_numeric)) %>%
    add_column(target = df$target) %>%
    recipe(target ~ .) %>%
    step_meanimpute(all_numeric()) %>%
    step_log(all_numeric(), signed = TRUE)

  # Positive variables transformations
  rec_bc <- df %>%
    select(one_of(vec_positive)) %>%
    add_column(target = df$target) %>%
    recipe(target ~ .) %>%
    step_meanimpute(all_numeric()) %>%
    step_BoxCox(all_numeric())

  rec_log <- df %>%
    select(one_of(vec_positive)) %>%
    add_column(target = df$target) %>%
    recipe(target ~ .) %>%
    step_meanimpute(all_numeric()) %>%
    step_log(all_numeric())

  # Juicing recipes
  juice_base <- juice(prep(rec_base, retain = TRUE))
  juice_yj   <- juice(prep(rec_yj, retain = TRUE))
  juice_logs <- juice(prep(rec_logs, retain = TRUE))
  juice_bc   <- juice(prep(rec_bc, retain = TRUE))
  juice_log  <- juice(prep(rec_log, retain = TRUE))

  # Calculating importance
  imp_base <- calculate_correlation(juice_base, method = "pearson") %>% filter(var_y == "target") %>% select(-var_y)
  imp_yj   <- calculate_correlation(juice_yj, method = "pearson") %>% filter(var_y == "target") %>% select(-var_y)
  imp_logs <- calculate_correlation(juice_logs, method = "pearson") %>% filter(var_y == "target") %>% select(-var_y)
  imp_bc   <- calculate_correlation(juice_bc, method = "pearson") %>% filter(var_y == "target") %>% select(-var_y)
  imp_log  <- calculate_correlation(juice_log, method = "pearson") %>% filter(var_y == "target") %>% select(-var_y)

  # Summarising results
  imp_summary_step <- imp_base %>%
    select(var_x, imp_base = cor) %>%
    left_join(imp_yj %>% select(var_x, imp_yj = cor), "var_x") %>%
    left_join(imp_logs %>% select(var_x, imp_logs = cor), "var_x") %>%
    left_join(imp_bc %>% select(var_x, imp_bc = cor), "var_x") %>%
    left_join(imp_log %>% select(var_x, imp_log = cor), "var_x") %>%
    gather(transformation, value, imp_base:imp_log) %>%
    rename(variable = var_x) %>%
    mutate(value = round(value, 3))

  imp_summary <- imp_summary_step %>%
    group_by(variable) %>%
    summarise(
      n_candidates = sum(value == max(abs(value), na.rm = TRUE), na.rm = TRUE),
      best_transformation = transformation[which.max(abs(value))],
      imp_max = max(abs(value), na.rm = TRUE)
    ) %>%
    left_join(filter(step, transformation == "imp_base") %>% select(-transformation), "variable") %>%
    rename(imp_base = value) %>%
    ungroup() %>%
    mutate(
      imp_base = abs(imp_base),
      imp_lift = imp_max - imp_base
    ) %>%
    arrange(desc(imp_lift))

  output <- list(
    recipes = list(
      rec_base = rec_base,
      rec_yj = rec_yj,
      rec_logs = rec_logs,
      rec_bc = rec_bc,
      rec_log = rec_log
    ),
    juice = list(
      juice_base = juice_base,
      juice_yj = juice_yj,
      juice_logs = juice_logs,
      juice_bc = juice_bc,
      juice_log = juice_log
    ),
    importance = list(
      imp_base = imp_base,
      imp_yj = imp_yj,
      imp_logs = imp_logs,
      imp_bc = imp_bc,
      imp_log = imp_log
    ),
    summary = imp_summary
  )

}

# Train predictive models -------------------------------------------------

#' Train various predictive models
#'
#' This function automatically builds different predictive models with reasonable default settings
#' based on the implementation in the caret package. Data preprocessing can happen automatically through applying
#' aider's default recipe blueprint. It is currently only implemented for classification problems. The
#' default resampling procedure is repeated cross-validation.
#'
#' @param df A data frame
#' @param target A target variable
#' @param type Specify the modelling task. Possible options are: "classification" (default) and "regression"
#' @param models Specify type of models to train. Possibile options are: "rf" (Random Forest) as default, as well as "en" (Elastic-Net), "svm" (Support Vector Machines) and "xgb" (XgBoost)
#' @param use_recipe Specify whether a standardized recipe should be applied. If FALSE then the dataset needs to pre-processed before applying the function. Defaults to TRUE
#' @param folds Specify the number of folds in cross-validation. Defaults to 5
#' @param repeats Specify the number of times the fitting process should be repeated. Defaults to 5
#' @param upsample Should the minority class be upsampled during resampling? Defaults to "yes"
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' models <- train_model(data, status)
#' @export
train_model <- function(df,
                        target,
                        type = "classification",
                        models = c("rf"),
                        use_recipe = TRUE,
                        folds = 5,
                        repeats = 5,
                        upsample = "yes"
                        ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_target <- enquo(target)

  df %<>%
    dplyr::rename(target = !!var_target)

  splits <- createMultiFolds(df$target, k = folds, times = repeats)

  ctrl <- trainControl(
    method = "repeatedcv",
    repeats = repeats,
    number = folds,
    index = splits,
    classProbs = TRUE,
    verboseIter = TRUE,
    returnResamp = "final",
    savePredictions = "final",
    search = "grid",
    allowParallel = TRUE
  )

  if (type == "classification") {
    ctrl$summaryFunction <- twoClassSummary
  } else {
    ctrl$summaryFunction <- defaultSummary
  }

  if (upsample == "yes") {
    ctrl$sampling <- "up"
  }

  if (use_recipe == TRUE) {
    recipe <- apply_recipe(df, target)
  } else {
    recipe <- recipe(target ~ ., df)
  }

  model_enet <- NA
  model_rf <- NA
  model_svm <- NA
  model_xgboost <- NA

  # Training an Elastic-net model
  if ("en" %in% models){

    message("Training an Elastic-net model")
    grid_enet <- expand.grid(
      alpha = c(0, .25, .50, .75, 1),
      lambda = 10 ^ seq(-4, 0, length = 30)
    )

    model_enet <- train(
      recipe,
      data = df,
      method = "glmnet",
      trControl = ctrl,
      tuneGrid = grid_enet
    )

  }

  # Training a Random Forest model
  if ("rf" %in% models){

    message("Training a Random Forest model")
    # grid_rf <- expand.grid(
    #   mtry = seq(2, ncol(df) / 3, length.out = 5)
    # )

    model_rf <- train(
      recipe,
      data = df,
      method = "ranger",
      trControl = ctrl,
      # tuneGrid = grid_rf,
      num.trees = 500,
      importance = "impurity"
    )

  }

  # Training an SVM model
  if ("svm" %in% models){

    message("Training an SVM model")
    grid_svm <- expand.grid(
      alpha = c(0, .25, .50, .75, 1),
      lambda = 10 ^ seq(-4, 0, length = 30)
    )

    model_svm <- train(
      recipe,
      data = df,
      method = "glmnet",
      trControl = ctrl,
      tuneGrid = grid_svm
    )

  }

  # Training Xgb
  if ("xgb" %in% models){

    message("Training an XgBoost model")
    grid_xgboost <- expand.grid(
      nrounds = c(25, 50, 100),
      max_depth = 6,
      eta = c(0.05, 0.1, 0.2, 0.3),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )

    model_xgboost <- train(
      recipe,
      data = df,
      method = "xgbTree",
      trControl = ctrl,
      tuneGrid = grid_xgboost
    )
  }

    output <- list(
      en  = if_model_enet,
      rf  = model_rf,
      svm = model_svm,
      xgb = model_xgboost
    )

}

# Assess model performance ------------------------------------------------

#' Assess model performance
#'
#' This function calculates the most importance classification task performance measures.
#'
#' @param df A data frame
#' @param actual A variable with actual outcome
#' @param prediction A variable with outcome prediction
#' @examples
#' assess_performance(credit_data, tbd)
#' @export
assess_performance <- function(df, actual = actual, prediction = prediction) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  var_actual     <- enquo(actual)
  var_prediction <- enquo(prediction)

  df_temp <- df %>%
    mutate(
      actual_fct       = as.factor(!!var_actual),
      prediction_num   = as.numeric(!!var_prediction),
      prediction_class = as.factor(ifelse(prediction_num >= 0.50, "1", "0"))
    )

  conf <- yardstick::conf_mat(df_temp, actual_fct, prediction_class)

  # We should also calculate Gini (gini <- round((2 * auc) - 1, 2))

  acc  <- yardstick::accuracy(df_temp, actual_fct, prediction_class)
  auc  <- yardstick::roc_auc(df_temp, actual_fct, prediction_num)
  mcc  <- yardstick::mcc(df_temp, actual_fct, prediction_class)

  sens <- yardstick::sens(df_temp, actual_fct, prediction_class)
  spec <- yardstick::spec(df_temp, actual_fct, prediction_class)

  ppv  <- yardstick::ppv(df_temp, actual_fct, prediction_class)
  npv  <- yardstick::npv(df_temp, actual_fct, prediction_class)

  pre  <- yardstick::precision(df_temp, actual_fct, prediction_class)
  rec  <- yardstick::recall(df_temp, actual_fct, prediction_class)

  outcome <- list(
    conf = conf,

    acc  = round(acc, 3),
    auc  = round(auc, 3),
    mcc  = round(mcc, 3),

    sens = round(sens, 3),
    spec = round(spec, 3),

    ppv  = round(ppv, 3),
    npv  = round(npv, 3),

    pre  = round(pre, 3),
    rec  = round(rec, 3)
  )

}
