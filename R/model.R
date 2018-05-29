
# Apply a bp1 model recipe ------------------------------------------------

#' Apply a bp1 model recipe
#'
#' This function calculates a bp1 model matrix recipe.
#'
#' @param df A data frame
#' @param target A target variable
#' @examples
#' apply_recipe_bp1(credit_data, Status)
#' @export
apply_recipe_bp1 <- function(df, target) {

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
      step_bagimpute(all_nominal(), -!!var_target) %>%
      step_other(all_nominal(), -!!var_target, threshold = .05, other = "other_values") %>%
      step_dummy(all_nominal(), -!!var_target)
  }

  recipe %<>%
    step_bagimpute(all_numeric()) %>%
    step_BoxCox(one_of(var_numeric)) %>%
    step_center(one_of(var_numeric)) %>%
    step_scale(one_of(var_numeric))

}

# Apply a bp2 model recipe ------------------------------------------------

#' Apply a bp2 model recipe
#'
#' This function calculates a bp2 model matrix recipe.
#'
#' @param df A data frame
#' @param target A target variable
#' @examples
#' apply_recipe_bp2(credit_data, Status)
#' @export
apply_recipe_bp2 <- function(df, target) {

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
      step_other(all_nominal(), -!!var_target, threshold = .05, other = "other_values") %>%
      step_dummy(all_nominal(), -!!var_target)
  }

  recipe %<>%
    step_meanimpute(all_numeric()) %>%
    step_BoxCox(one_of(var_numeric)) %>%
    step_center(one_of(var_numeric)) %>%
    step_scale(one_of(var_numeric))

}

# Apply a bp3 model recipe ------------------------------------------------

#' Apply a bp3 model recipe
#'
#' This function calculates a bp3 model matrix recipe.
#'
#' @param df A data frame
#' @param target A target variable
#' @examples
#' apply_recipe_bp3(credit_data, Status)
#' @export
apply_recipe_bp3 <- function(df, target) {

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
      step_other(all_nominal(), -!!var_target, threshold = .05, other = "other_values") %>%
      step_dummy(all_nominal(), -!!var_target)
  }

  recipe %<>%
    step_meanimpute(all_numeric()) %>%
    step_BoxCox(one_of(var_numeric))

}

# Build predictive models -------------------------------------------------

#' Build various predictive models
#'
#' This function specifies different predictive models on the dataset. Data preprocessing happens automatically through applying the BP2 recipe blueprint.
#'
#' @param df A data frame
#' @param target A target variable
#' @param type Specify the modelling task. Possible options are: "classification" (default) and "regression"
#' @param models Specify type of models to train. Possibile options are: "en" (Elastic-Net) and "rf" (Random Forest) as default, as well as "svm" (Support Vector Machines) and "xgb" (XgBoost)
#' @param folds Specify the number of folds in cross-validation. Defaults to 5
#' @param repeats Specify the number of times the fitting process should be repeated. Defaults to 1
#' @param upsample Should the minority class be upsampled during resampling? Defaults to "no"
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' models <- train_model(data, status, upsample)
#' @export
train_model <- function(df,
                        target,
                        type = "classification",
                        models = c("en", "rf"),
                        folds = 5,
                        repeats = 1,
                        upsample = "no"
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

  recipe <- apply_recipe_bp2(df, target)

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
    grid_rf <- expand.grid(
      ntree = 500,
      .mtry = if (type == "classification") {sqrt(ncol(df))} else {ncol(df) / 3}
    )

    model_rf <- train(
      recipe,
      data = df,
      method = "rfr",
      trControl = ctrl,
      tuneGrid = grid_rf
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

  # Training Xgm
  if ("xgb" %in% models){

    message("Training an XgBoost model")
    grid_xgboost <- expand.grid(
      nrounds = 100,
      max_depth = 6,
      eta = 0.3,
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
    enet = model_enet,
    rforest = model_rf,
    svm = model_svm,
    xgboost = model_xgboost
  )

}

# Find meaningfull interactions -------------------------------------------

#' Find meaningful variable interactions
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
#' int <- find_interactions(data, status, upsample = "yes")
#' @export
find_interactions <- function(df,
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
  recipe_enet <- apply_recipe_bp2(df, target) %>%
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

# Find most predictive variables ------------------------------------------

#' Find most predictive variables
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
#' find_most_predictive(data, status, 0.5)
#' @export
find_most_predictive <- function(df,
                                 target,
                                 cutoff) {

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

# Compare samples ---------------------------------------------------------

#' Compare samples similarity
#'
#' This function estimated a model to compare two samples similarity and the most predictive variables.
#'
#' @param df A a data frame
#' @param times Number of resample repetitions. Defaults to 1. For tasks that require more precise results this number can be increased to e.g. 5 (note that could significantly increase computation time)
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' data_v1 <- data %>%
#'   select(-status) %>%
#'   filter(records %in% c("yes")) %>%
#'   mutate(status = 1)
#'
#' data_v2 <- data %>%
#'   select(-status) %>%
#'   filter(records %in% c("no")) %>%
#'   mutate(status = 0)
#'
#' data_cmb <- bind_rows(
#'   data_v1,
#'   data_v2
#'   ) %>%
#'   mutate(status = ifelse(status == 1, "yes", "no")) %>%
#'   rename(target = status) %>%
#'   compare_samples()
#' @export
compare_samples <- function(df, times = 1) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  recipe <- apply_recipe_bp1(df, target)

  splits <- createMultiFolds(df$target, k = 5, times = 1)

  grid <- expand.grid(
    alpha = seq(0, 1, by = .25),
    lambda = 10 ^ seq(-4, 0, length = 20)
  )

  ctrl <- trainControl(
    method = "repeatedcv",
    repeats = 5,
    number = 5,
    index = splits,
    classProbs = TRUE,
    verboseIter = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )

  model <- train(
    recipe,
    data = df,
    method = "glmnet",
    trControl = ctrl,
    tuneGrid = grid
  )

  results <- model$results %>%
    arrange(desc(ROC)) %>%
    slice(1:10)

  importance <- varImp(model, scale = FALSE)[[1]] %>%
    tibble::rownames_to_column("var") %>%
    rename(imp = Overall) %>%
    mutate(
      imp_norm = (imp - min(imp)) / (max(imp) - min(imp)),
      imp_norm = formattable::percent(imp_norm)
    ) %>%
    arrange(desc(imp))

  plot_importance <- importance %>%
    top_n(n = 15, imp) %>%
    mutate(n = round(n, 2)) %>%
    ggplot(aes(reorder(var, imp_norm), imp_norm, fill = imp_norm)) +
    geom_bar(stat = "identity", width = .9, alpha = 1, color = "black", size = .1) +
    geom_text(
      aes(label = imp_norm, y = imp_norm + 0.05),
      position = position_dodge(.9),
      size = 3.5,
      check_overlap = T
    ) +
    labs(
      title = "Relative variables importance",
      fill = "Importance:",
      x = "Variable",
      y = "Importance") +
    aider_theme() +
    scale_fill_gradient(low = c("#BFEFFF"), high = c("#009ACD"), guide = "colorbar") +
    coord_flip()

  outcome <- list(
    results = results,
    importance = importance,
    plot_importance = plot_importance
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

# Identify outliers -------------------------------------------------------

#' Identify outliers
#'
#' This function calculates individual observations outlier score with the Lof algorithm and
#' returns the original data frame with the Lof outlier score, Lof stats and variables with the
#' highest impact on the Lof score, as well as a data frame with the most outlying cases (approx. top 5%).
#'
#' @param df A data frame
#' @examples
#' data <- recipes::credit_data %>%
#'   apply_recipe_bp2(Status) %>%
#'   prep(retain = TRUE) %>%
#'   juice() %>%
#'   select(-Status)
#'
#' out <- identify_outliers(data)
#' @export
identify_outliers <- function(df) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  out_scores <- Rlof::lof(df, c(5:10)) %>%
    as_data_frame() %>%
    rowwise() %>%
    mutate(
      lof = median(c(`5`, `6`, `7`, `8`, `9`, `10`))
    ) %>%
    select(lof)

  df %<>%
    add_column(lof = out_scores$lof, .before = 1)

  lof_stats <- calculate_stats_numeric(df["lof"])
  lof_imp   <- calculate_importance(df, lof, "regression")

  df_out <- df %>%
    filter(lof >= lof_stats$avg + lof_stats$std * 3) %>%
    arrange(desc(lof))

  outcome <- list(
    df = df,
    lof_stats = lof_stats,
    lof_imp = lof_imp,
    df_out = df_out
    )

}
