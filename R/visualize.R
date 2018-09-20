
# Number ticks ------------------------------------------------------------

#' Equal sized axis ticks
#'
#' This function creates equally spaced axis ticks for ggplot graphs. Should be used as input
#' for the "break" argument of scale_continuous function in a ggplot function.
#'
#' @param n Number of ticks
#' @export
number_ticks <- function(n = 10) {

  if (!is.numeric(n))
    stop("n must be a numeric input")

  function(limits) {
    pretty(limits, n)
  }

}

# Aider theme -------------------------------------------------------------

#' Aider ggplot2 theme
#'
#' This function applies the aider theme to any ggplot graph in order to
#' create more complete and nicer looking visualizations.
#'
#' @param type Select a theme type. Defaults to "grey", another option includes also "ipsum". Otherwise no theme is applied
#'
#' @export
aider_theme <- function(type = "grey") {

  if (type == "grey") {
    theme_grey() +
      theme(
        title        = element_text(size = rel(.9)),
        plot.title   = element_text(face = "bold"),
        axis.title.x = element_text(colour = "black", face = "bold"),
        axis.title.y = element_text(colour = "black", face = "bold"),
        axis.text.x  = element_text(colour = "black"),
        axis.text.y  = element_text(colour = "black"),
        panel.border = element_rect(colour = "#4c4c4c", fill = NA),
        strip.text.x = element_text(colour = "black", face = "bold"),
        strip.background = element_rect(colour = "#4c4c4c", fill = "#cccccc")
      )
  } else if (type == "ipsum") {
    hrbrthemes::theme_ipsum() +
      theme(
        title        = element_text(size = rel(0.9)),
        plot.title   = element_text(colour = "black", face = "bold", size = rel(0.8)),
        axis.title.x = element_text(colour = "black", size = rel(1.05)),
        axis.title.y = element_text(colour = "black", size = rel(1.05)),
        strip.text.x = element_text(colour = "black", size = rel(1.1)),
        legend.title = element_text(colour = "black", face = "bold")
      )
  } else {
    theme(
      title        = element_text(size = rel(.9)),
      plot.title   = element_text(face = "bold"),
      axis.title.x = element_text(colour = "black", face = "bold"),
      axis.title.y = element_text(colour = "black", face = "bold"),
      axis.text.x  = element_text(colour = "black"),
      axis.text.y  = element_text(colour = "black"),
      panel.border = element_rect(colour = "#4c4c4c", fill = NA),
      strip.text.x = element_text(colour = "black", face = "bold"),
      strip.background = element_rect(colour = "#4c4c4c", fill = "#cccccc")
    )
  }

}

# Select palette ----------------------------------------------------------

#' Palettes are based on the list of available color schemes: https://github.com/EmilHvitfeldt/r-color-palettes. We selected a shortlist of the most sensible palettes for you.
#'
#' @param palette Select a palette. Available options are: use "risk" for approved/ rejected, performing/ non-performing palletes, use "cartography" to get
#' 20 discrete colors or "awtools" to get 8 discrete colors, and finally use "berlin", "lajolla" or "redgreen" to get 60 continuous colors. Defaults to "cartography"
#'
#' @export
select_palette <- function(palette = "cartography"){

  if (!is.character(palette))
    stop("argument must be character")

  if (palette == "risk") {

    c(
      "0" = "#40C157",
      "1" = "#F4675C",

      "Pl" = "#40C157",
      "Npl" = "#F4675C",

      "Approved" = "#40C157",
      "Rejected" = "#F4675C"
    )

  # Discrete palettes

  } else if (palette == "cartography") {

    cartography::carto.pal(pal1 = "blue.pal", n1 = 10, pal2 = "sand.pal", n2 = 10)

  } else if (palette == "awtools") {

    paletteer::paletteer_d("awtools", "a_palette")

  # Continuous palettes

  } else if (palette == "berlin") {

    paletteer::paletteer_c("scico", "berlin", 60)

  } else if (palette == "lajolla") {

    paletteer::paletteer_c("scico", "lajolla", 60)

  } else if (palette == "redgreen") {

    grDevices::colorRampPalette(c("#99ff99", "#ffd27f", "#ff4c4c"))(60)

  } else if (palette == "greenred") {

    grDevices::colorRampPalette(c("#ff4c4c", "#ffd27f", "#99ff99"))(60)

  } else {
    NULL
  }
}

# Create a density plot ---------------------------------------------------

#' Plot density of numerical variables
#'
#' This function creates nicely formatted, standardised density plots.
#'
#' @param df A data frame
#' @param x A numerical variable to plot its density
#' @param fill Select an additional grouping variable to be used for density plotting. Defaults to NULL
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL
#' @param ticks Select the number of ticks on the x and y axis. Defaults to 10
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0
#' @param title Should the plot title appear automatically. Defaults to TRUE
#' @param subtitle Text that is displayed on the subtitle. Defaults to NULL
#' @param caption Text that is displayed on the caption. Defaults to NULL
#' @param lab_x Text that is displayed on the x axis. Defaults to "Value range"
#' @param lab_y Text that is displayed on the y axis. Defaults to "Density"
#' @param legend Should the plot legend appear automatically. Defaults to TRUE
#' @param vline Should any vertical lines be added to the plot. Defaults to c(NaN)
#' @param alpha Select plot fill transparency. Defaults to .5
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 2.5\%
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 97.5\%
#' @param palette Select a color palette from colors available in the select_palette function
#' @param theme_type Select a theme type from themes available in the aider_theme function
#'
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' data %>%
#'   plot_density(x = time)
#'
#' data %>%
#'   plot_density(x = time,
#'                facet = home)
#'
#' data %>%
#'   plot_density(x = time,
#'                fill = home,
#'                facet = home,
#'                ticks = 10,
#'                title = TRUE,
#'                legend = TRUE,
#'                alpha = .5)
#' @export

plot_density <- function(df,
                         x,
                         fill = NULL,
                         facet = NULL,
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         subtitle = NULL,
                         caption = NULL,
                         lab_x = "Value range",
                         lab_y = "Density",
                         legend = TRUE,
                         vline = c(NaN),
                         alpha = .7,
                         quantile_low = .025,
                         quantile_high = .975,
                         palette = "cartography",
                         theme_type = "grey"
                         ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(palette))
    stop("argument must be character")

  var_x     <- enquo(x)
  var_fill  <- enquo(fill)
  var_facet <- enquo(facet)

  limits <- df %>%
    select(value = !!var_x) %>%
    summarise(
      min = quantile(value, quantile_low[[1]], na.rm = TRUE),
      max = quantile(value, quantile_high[[1]], na.rm = TRUE)
    )

  plot <- df %>%
    ggplot() +
    geom_vline(xintercept = vline, linetype = 2, size = 1, color = "#6E7B8B", alpha = .8) +
    ggtitle(label = ifelse(title == TRUE, glue("Density plot of {rlang::quo_text(var_x)}"),
                           ifelse(is.character(title), title, element_blank()))) +
    labs(
      fill = glue("{first_to_upper(rlang::quo_text(var_fill))}:"),
      x = lab_x,
      y = lab_y) +
    labs(
      subtitle = ifelse(is.null(subtitle), element_blank(), subtitle)
    ) +
    labs(
      caption = ifelse(is.null(caption), element_blank(), caption)
    ) +
    scale_x_continuous(
      limits = c(
        limits$min,
        limits$max
      ),
      breaks = number_ticks(ticks)
    ) +
    scale_y_continuous(
      breaks = number_ticks(ticks)
    ) +
    aider_theme(type = theme_type) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    )

  if (!rlang::quo_is_null(var_facet)) {
    plot <- plot +
      facet_wrap(rlang::quo_text(var_facet), scales = "free_x")
  }

  if (rlang::quo_is_null(var_fill)) {

    message("Wow, what a beautiful graph!")
    plot +
      geom_density(
        aes_string(
          x = rlang::quo_text(var_x)
        ),
        alpha = alpha,
        fill = "#1d8fd2"
      )

  } else {

    levels <- df %>%
      select(levels = !!var_fill)

    selected_palette <- select_palette(palette) %>%
      as_data_frame() %>%
      mutate(
        rank = row_number(),
        fill = rank %% (round(n() / length(unique(levels$levels)), 0))
        ) %>%
      filter(fill == 0)

    message("Damn, this graph is amazing!")

    plot +
      geom_density(
        aes_string(
          x = rlang::quo_text(var_x),
          fill = rlang::quo_text(var_fill)
        ),
        alpha = alpha
      ) +
      scale_fill_manual(values = selected_palette$value)
  }

}

# Create a boxplot ---------------------------------------------------

#' Plot box-plots of numerical variables
#'
#' This function creates nicely formatted, standardised box-plots.
#'
#' @param df A data frame
#' @param x A categorical variable for the x axis groupings
#' @param y A numerical variable for the y axis levels
#' @param fill Select an additional grouping variable to be used for plotting. Defaults to NULL
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL
#' @param ticks Select the number of ticks on the y axis. Defaults to 10
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0
#' @param title Should the plot title appear automatically. Defaults to TRUE
#' @param subtitle Text that is displayed on the subtitle. Defaults to NULL
#' @param caption Text that is displayed on the caption. Defaults to NULL
#' @param lab_x Text that is displayed on the x axis. Defaults to "Level"
#' @param lab_y Text that is displayed on the y axis. Defaults to "Value range"
#' @param legend Should the plot legend appear automatically. Defaults to TRUE
#' @param vline Should any horizontal lines be added to the plot. Defaults to c(NaN)
#' @param alpha Select plot fill transparency. Defaults to .7
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 2.5\%
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 97.5\%
#' @param palette Select a color palette from colors available in the select_palette function
#' @param theme_type Select a theme type from themes available in the aider_theme function
#' @examples
#' data <- credit_data %>%
#'   first_to_lower()
#'
#' data %>%
#'   plot_boxplot(x = marital,
#'                y = time)
#'
#' data %>%
#'   plot_boxplot(x = marital,
#'                y = time,
#'                fill = marital)
#'
#' data %>%
#'   plot_boxplot(x = marital,
#'                y = time,
#'                fill = marital,
#'                facet = job)
#'
#' data %>%
#'   plot_boxplot(x = marital,
#'                y = time,
#'                fill = marital,
#'                facet = job,
#'                ticks = 5,
#'                vline = 45,
#'                angle = 45,
#'                alpha = .7)
#' @export
plot_boxplot <- function(df,
                         x,
                         y,
                         fill = NULL,
                         facet = NULL,
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         subtitle = NULL,
                         caption = NULL,
                         lab_x = "Level",
                         lab_y = "Value range",
                         legend = TRUE,
                         vline = c(NaN),
                         alpha = .7,
                         quantile_low = .025,
                         quantile_high = .975,
                         palette = "cartography",
                         theme_type = "grey"
                         ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(palette))
    stop("argument must be character")

  var_x     <- enquo(x)
  var_y     <- enquo(y)
  var_fill  <- enquo(fill)
  var_facet <- enquo(facet)

  limits <- df %>%
    select(value = !!var_y) %>%
    summarise(
      min = quantile(value, quantile_low[[1]], na.rm = TRUE),
      max = quantile(value, quantile_high[[1]], na.rm = TRUE)
    )

  plot <- df %>%
    ggplot() +
    geom_hline(yintercept = vline, linetype = 2, size = 1, color = "#6E7B8B", alpha = .8) +
    ggtitle(label = ifelse(title == TRUE, glue("Boxplot plot of {rlang::quo_text(var_y)} by {rlang::quo_text(var_x)}"),
                           ifelse(is.character(title), title, element_blank()))) +
    labs(
      fill = glue("{first_to_upper(rlang::quo_text(var_fill))}:"),
      x = lab_x,
      y = lab_y) +
    labs(
      subtitle = ifelse(is.null(subtitle), element_blank(), subtitle)
    ) +
    labs(
      caption = ifelse(is.null(caption), element_blank(), caption)
    ) +
    scale_y_continuous(
      limits = c(
        limits$min,
        limits$max
      ),
      breaks = number_ticks(ticks)
    ) +
    aider_theme(type = theme_type) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    )

  if (!rlang::quo_is_null(var_facet)) {
    plot <- plot +
      facet_wrap(rlang::quo_text(var_facet), scales = "free_x")
  }

  if (rlang::quo_is_null(var_fill)) {

    message("Wow, what a beautiful graph!")
    plot +
      geom_boxplot(
        aes_string(
          x = rlang::quo_text(var_x),
          y = rlang::quo_text(var_y)
        ),
        alpha = alpha,
        fill = "#1d8fd2"
      )

  } else {

    levels <- df %>%
      select(levels = !!var_fill)

    selected_palette <- select_palette(palette) %>%
      as_data_frame() %>%
      mutate(
        rank = row_number(),
        fill = rank %% (round(n() / length(unique(levels$levels)), 0))
      ) %>%
      filter(fill == 0)

    message("Damn, this graph is amazing!")
    plot +
      geom_boxplot(
        aes_string(
          x = rlang::quo_text(var_x),
          y = rlang::quo_text(var_y),
          fill = rlang::quo_text(var_fill)
        ),
        alpha = alpha
      ) +
      scale_fill_manual(values = selected_palette$value)
  }

}

# Create a decile plot ---------------------------------------------------

#' Plot decile plots of numerical variables
#'
#' This function creates nicely formatted, standardised decile plots. Prior to calling the function
#' the data should only be in a form of a decile table (calculate_decile_table() function will
#' do that for you).
#'
#' @param df A data frame
#' @param x A categorical variable for the x axis groupings. Defaults to 'decile'
#' @param y A numerical variable for the y axis levels. Defaults to 'bad_rate'
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL
#' @param ticks Select the number of ticks on the y axis. Defaults to 10
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0
#' @param title Should the plot title appear automatically. Defaults to TRUE
#' @param subtitle Text that is displayed on the subtitle. Defaults to NULL
#' @param caption Text that is displayed on the caption. Defaults to NULL
#' @param lab_x Text that is displayed on the x axis. Defaults to "Decile"
#' @param lab_y Text that is displayed on the y axis. Defaults to "Value range"
#' @param legend Should the plot legend appear automatically. Defaults to TRUE
#' @param alpha Select plot fill transparency. Defaults to .7
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 2.5\%
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 97.5\%
#' @param palette Select a color palette from colors available in the select_palette function
#' @param theme_type Select a theme type from themes available in the aider_theme function
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_decile_table(binning = age,
#'                          grouping = status,
#'                          top_level = "bad") %>%
#'   plot_deciles()
#' @export
plot_deciles <- function(df,
                         x = decile,
                         y = bad_rate,
                         facet = NULL,
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         subtitle = NULL,
                         caption = NULL,
                         lab_x = "Decile",
                         lab_y = "Value range",
                         legend = TRUE,
                         alpha = .7,
                         quantile_low = .025,
                         quantile_high = .975,
                         palette = "redgreen",
                         theme_type = "grey"
                         ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(palette))
    stop("argument must be character")

  var_x     <- enquo(x)
  var_y     <- enquo(y)
  var_facet <- enquo(facet)

  limits_min <- 0
  limits_max <- select(df, !!var_y)[[1]] %>% max() + .05

  selected_palette <- select_palette(palette) %>%
    as_data_frame()

  message("Wow, what a beautiful graph!")
  plot <- df %>%
    ggplot() +
    geom_bar(
      aes(
        x = decile,
        y = bad_rate,
        fill = bad_rate
      ),
      stat = "identity",
      width = .8,
      alpha = alpha
    ) +
    geom_text(
      aes(
        x = decile,
        y = bad_rate + 0.01,
        label = round(median, 2)
      ),
      position = position_dodge(.9),
      size = 3.2,
      check_overlap = T
    ) +
    ggtitle(label = ifelse(title == TRUE, glue("Decile plot of {rlang::quo_text(var_y)} by {rlang::quo_text(var_x)}"),
                           ifelse(is.character(title), title, element_blank()))) +
    labs(
      fill = "Ratio",
      x = lab_x,
      y = lab_y) +
    labs(
      subtitle = ifelse(is.null(subtitle), element_blank(), subtitle)
    ) +
    labs(
      caption = ifelse(is.null(caption), element_blank(), caption)
    ) +
    scale_y_continuous(
      limits = c(
        limits_min,
        limits_max
      ),
      labels = scales::percent,
      breaks = number_ticks(ticks)
    ) +
    aider_theme(type = theme_type) +
    scale_fill_gradientn(colours = selected_palette$value) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    )

  if (!rlang::quo_is_null(var_facet)) {
    plot +
      facet_wrap(rlang::quo_text(var_facet), scales = "free_x")
  }

  plot

}

# Create a calibration plot -----------------------------------------------

#' Plot a calibration plot of model performance
#'
#' This function creates a nicely formatted, standardised calibration plot. Prior to calling the function
#' the data should only be in a form of a decile table (calculate_decile_table() function will
#' do that for you), unless it's already provided.
#'
#' @param df A data frame
#' @param title Text that is displayed on as the plot title. Defaults to "Lift chart: evaluation of model predicted probabilities vs. actual defaul rates across deciles"
#' @param lab_x Text that is displayed on the x axis. Defaults to "Deciles of predicted probabilities"
#' @param lab_y Text that is displayed on the y axis. Defaults to "Decile performance"
#' @examples
#' df <- tibble::tribble(
#'   ~decile, ~actual_br, ~predicted_br,
#'   1,  0.00, 0.01,
#'   2,  0.00, 0.01,
#'   3,  0.00, 0.03,
#'   4,  0.00, 0.05,
#'   5,  0.30, 0.08,
#'   6,  0.12, 0.11,
#'   7,  0.00, 0.16,
#'   8,  0.21, 0.22,
#'   9,  0.30, 0.33,
#'   10, 0.68, 0.59,
#' )
#'
#' plot_calibration(df)
#' @export
plot_calibration <- function(df,
                         title = "Lift chart: evaluation of model predicted probabilities vs. actual defaul rates across deciles",
                         lab_x = "Deciles of predicted probabilities",
                         lab_y = "Decile performance") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  limits_min <- 0
  limits_max <- select(df, actual_br)[[1]] %>% max() + .05

  message("Wow, what a beautiful graph!")
  plot <- df %>%
    ggplot(aes(decile, actual_br)) +
    geom_smooth( # actual
      stat = "smooth",
      se = FALSE,
      color = "blue",
      size = 1.5,
      span = .6
      ) +
    geom_smooth( # predicted
      aes(decile, predicted_br),
      stat = "smooth",
      se = FALSE,
      color = "red",
      size = 1.5,
      span = .6
      ) +
    labs(
      title = title,
      subtitle = "
        Blue: actual
        Red: predicted",
      x = lab_x,
      y = lab_y
    ) +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(limits_min, limits_max),
      breaks = number_ticks(10)
      ) +
    scale_x_continuous(
      breaks = number_ticks(10)
      ) +
    aider_theme()

}

# Create log-odds plot ----------------------------------------------------

#' Plot a log-odds table
#'
#' This function creates a nicely formatted, standardised log-odds plot. Prior to calling the function
#' the data should only be in a form of a log-odds table (calculate_logodds_table() function will
#' do that for you), unless it's already provided.
#'
#' @param df A data frame
#' @param title Text that is displayed on as the plot title. Defaults to "Lift chart: evaluation of model predicted probabilities vs. actual defaul rates across deciles"
#' @param lab_x Text that is displayed on the x axis. Defaults to "Mean of variable deciles"
#' @param lab_y Text that is displayed on the y axis. Defaults to "Log-odds"
#' @examples
#' credit_data %>%
#'   first_to_lower() %>%
#'   calculate_logodds_table(binning = time,
#'                           grouping = status,
#'                           top_level = "bad") %>%
#'   plot_logodds()
#' @export
plot_logodds <- function(df,
                         title = "Evaluation of log-odds linearity",
                         lab_x = "Mean of variable deciles",
                         lab_y = "Log-odds") {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  message("Hey girl, what are the odds?")
  plot <- df %>%
    ggplot(aes(mean, log_odds)) +
    geom_point(
      shape = 21,
      colour = "black",
      fill = "white",
      size = 1,
      stroke = 1.1
      ) +
    geom_smooth(
      method = lm,
      se = FALSE,
      color = "blue",
      size = 1.5,
      span = 1
    ) +
    labs(
      title = title,
      x = lab_x,
      y = lab_y
    ) +
    scale_y_continuous(
      breaks = number_ticks(10)
    ) +
    scale_x_continuous(
      breaks = number_ticks(10)
    ) +
    aider_theme()

  return(plot)

}

# Create a correlation matrix ---------------------------------------------

#' Plot a correlation matrix of numerical variables
#'
#' This function creates a nicely formatted, standardised correlation matrix of numerical variables. Long variables names should be shortened before for easier interpretation.
#'
#' @param df A data frame
#' @param method A character string indicating which correlation coefficient (or covariance) is to be computed. One of "spearman" (default), "pearson" or "kendall": can be abbreviated
#' @param order Ordering method of the correlation matrix. Recommended options are: "alphabet" (default) and "hclust"
#' @param label_size Size of the text label. Defaults to 0.7
#' @param number_size Size of the correlation number. Defaults to 0.9
#' @examples
#' credit_data %>% plot_correlation()
#' @export
plot_correlation <- function(df,
                             method = "spearman",
                             order = "alphabet",
                             label_size = 0.7,
                             number_size = 0.9) {

  ### Testing
  # df <- credit_data
  # method = "spearman"
  # order = "hclust"
  # label_size = 0.7
  ###

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (any(!is.character(method), !is.character(order)))
    stop("arguments must be character")

  if (!is.numeric(label_size))
    stop("argument must be numeric")

  message("Holly cow, that's mindblowing!")
  cor_mtx <- df %>%
    select_if(is.numeric) %>%
    cor(use = "pairwise.complete.obs", method = method)

  cor_sig <- corrplot::cor.mtest(cor_mtx, conf.level = .95)

  corrplot::corrplot(
    cor_mtx,
    col = colorRampPalette(c("#6666ff","white","#ff4c4c"))(200),
    order = order,
    tl.cex = label_size,
    addCoef.col = "black",
    number.cex = number_size,
    method = "square",
    type = "lower",
    tl.pos = "dt",
    addrect = 3,
    tl.col = "black",
    tl.srt = 45,
    p.mat = if (order == "alphabet") {NULL} else {cor_sig$p},
    insig = "blank",
    diag = FALSE)

}

# Create a barplot ---------------------------------------------------

#' Plot bar-plots of numerical variables
#'
#' This function creates nicely formatted, standardised bar-plots.
#'
#' @param df A data frame
#' @param x A categorical variable for the x axis groupings
#' @param fill Select an additional grouping variable to be used for plotting. Defaults to NULL
#' @param facet Select an additional faceting variable to create facets. Defaults to NULL
#' @param binwidth Select binwidth, defaults to NULL
#' @param position Select the position of the barplot. Defaults to "dodge"
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0
#' @param title Should the plot title appear automatically. Defaults to TRUE
#' @param subtitle Text that is displayed on the subtitle. Defaults to NULL
#' @param caption Text that is displayed on the caption. Defaults to NULL
#' @param lab_x Text that is displayed on the x axis. Defaults to "Level"
#' @param lab_y Text that is displayed on the y axis. Defaults to "Value range"
#' @param legend Should the plot legend appear automatically. Defaults to TRUE
#' @param vline Should any horizontal lines be added to the plot. Defaults to c(NaN)
#' @param alpha Select plot fill transparency. Defaults to 1
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 2.5\%
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 97.5\%
#' @param palette Select a color palette from colors available in the select_palette function
#' @param theme_type Select a theme type from themes available in the aider_theme function
#' @examples
#'
#'data <- credit_data %>%
#'  first_to_lower()
#'
#'data %>%
#'  plot_bars(x = income)
#'
#'data %>%
#'  plot_bars(x = income,
#'            fill = marital)
#'data %>%
#'  plot_bars(x = income,
#'            fill = marital,
#'            facet = job)
#'data %>%
#'  plot_bars(x = income,
#'            fill = marital,
#'            facet = job,
#'            position = "stack",
#'            binwidth = 50,
#'            vline = 45,
#'            angle = 45,
#'            alpha = .7,
#'            palette = "berlin")
#'
#'
#' @export
plot_bars <- function(df,
                      x,
                      fill = NULL,
                      facet = NULL,
                      binwidth = NULL,
                      position = "dodge",
                      angle = 0,
                      title = TRUE,
                      subtitle = NULL,
                      caption = NULL,
                      lab_x = "Value range",
                      lab_y = "Count",
                      legend = TRUE,
                      vline = c(NaN),
                      alpha = 1,
                      quantile_low = .025,
                      quantile_high = .975,
                      palette = "cartography",
                      theme_type = "grey"
                      ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  if (!is.character(palette))
    stop("argument must be character")

  var_x     <- enquo(x)
  var_fill  <- enquo(fill)
  var_facet <- enquo(facet)

  limits <- df %>%
    select(value = !!var_x) %>%
    summarise(
      min = quantile(value, quantile_low[[1]], na.rm = TRUE),
      max = quantile(value, quantile_high[[1]], na.rm = TRUE)
    )

  plot <- df %>%
    ggplot() +
    geom_vline(xintercept = vline, linetype = 2, size = 1, color = "#6E7B8B", alpha = .8) +
    ggtitle(label = ifelse(title == TRUE, glue("Bar plot of {rlang::quo_text(var_x)}"),
                           ifelse(is.character(title), title, element_blank()))) +

    labs(
      fill = glue("{first_to_upper(rlang::quo_text(var_fill))}:"),
      x = lab_x,
      y = lab_y
      ) +
    labs(
      subtitle = ifelse(is.null(subtitle), element_blank(), subtitle)
    ) +
    labs(
      caption = ifelse(is.null(caption), element_blank(), caption)
    ) +
    aider_theme(type = theme_type) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    )

  if (!rlang::quo_is_null(var_facet)) {
    plot <- plot +
      facet_wrap(rlang::quo_text(var_facet), scales = "free_x")
  }

  if (rlang::quo_is_null(var_fill)) {

    message("Wow, what a beautiful graph!")

    plot +
      geom_histogram(
        aes_string(
          x = rlang::quo_text(var_x)
        ),
        alpha = alpha,
        position = position,
        fill = "#1d8fd2",
        binwidth  = binwidth
      ) +
      xlim(limits$min, limits$max)

  } else {

    levels <- df %>%
      select(levels = !!var_fill)

    selected_palette <- select_palette(palette) %>%
      as_data_frame() %>%
      mutate(
        rank = row_number(),
        fill = rank %% (round(n() / length(unique(levels$levels)), 0))
      ) %>%
      filter(fill == 0)

    message("Damn, this graph is amazing!")
    plot +
      geom_histogram(
        aes_string(
          x = rlang::quo_text(var_x),
          fill = rlang::quo_text(var_fill)
        ),
        alpha = alpha,
        position = position,
        binwidth = binwidth
      ) +
      xlim(limits$min, limits$max) +
      scale_fill_manual(values = selected_palette$value)
  }

}

