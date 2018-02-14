
# Number ticks ------------------------------------------------------------

#' Equal sized axis
#'
#' This function created equally spaced axis ticks for ggplot graphs.
#'
#' @param n Number of ticks
#' @keywords ticks
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
#' This function applies the aider package theme to any ggplot graph. It is based on theme_grey.
#'
#' @keywords theme
#' @export
aider_theme <- function() {
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
}

# Create a density plot ---------------------------------------------------

#' Create a density plot
#'
#' This function creates nicely formatted, standardised density plots.
#'
#' @param df A data frame
#' @param x A numerical variable to plot its density
#' @param fill Select an additional grouping variable to be used for density plotting. Defaults to NULL
#' @param facet Select an additional faceting variables to create facets. Defaults to c(" ")
#' @param ticks Select the number of ticks on the x and y axis. Defaults to 10
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0
#' @param title Should the plot title appear automatically. Defaults to TRUE
#' @param legend Should the plot legend appear automatically. Defaults to TRUE
#' @param vline Should any vertical lines be added to the plot. Defaults to c(Inf)
#' @param alpha Select plot transparency. Defaults to .5
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 2.5%
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 97.5%
#' @keywords plot
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   plot_density(x = in_sales_v1)
#'
#' aider_data_v1 %>%
#'   plot_density(x = in_sales_v1,
#'                facet = country)
#'
#' aider_data_v1 %>%
#'   plot_density(x = in_sales_v1,
#'                fill = country,
#'                facet = country,
#'                ticks = 10,
#'                title = TRUE,
#'                vline = 1500000,
#'                legend = TRUE,
#'                alpha = .5)
#' @export
plot_density <- function(df,
                         x,
                         fill = NULL,
                         facet = c(" "),
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         legend = TRUE,
                         vline = c(Inf),
                         alpha = .7,
                         quantile_low = .025,
                         quantile_high = .975
                         ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  get_aider_palletes()

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
    ggtitle(label = ifelse(title == TRUE, glue::glue("Density plot of {rlang::quo_text(var_x)}"), element_blank())) +
    labs(
      fill = glue::glue("{first_to_upper(rlang::quo_text(var_fill))}:"),
      x = "Value range",
      y = "Density") +
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
    aider_theme() +
    scale_fill_manual(values = wesanderson::wes_palette("Royal1")) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    ) +
    facet_wrap(rlang::quo_text(var_facet))

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

  message("Deam, this graph is amazing!")
  plot +
    geom_density(
      aes_string(
        x = rlang::quo_text(var_x),
        fill = rlang::quo_text(var_fill)
        ),
      alpha = alpha
      )
  }
}

# Create a boxplot ---------------------------------------------------

#' Create a boxplot
#'
#' This function creates nicely formatted, standardised box plots.
#'
#' @param df A data frame
#' @param x A categorical variable for the x axis groupings
#' @param y A numerical variable for the y axis levels
#' @param fill Select an additional grouping variable to be used for plotting. Defaults to NULL
#' @param facet Select an additional faceting variables to create facets. Defaults to c(" ")
#' @param ticks Select the number of ticks on the y axis. Defaults to 10
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0
#' @param title Should the plot title appear automatically. Defaults to TRUE
#' @param legend Should the plot legend appear automatically. Defaults to TRUE
#' @param vline Should any horizontal lines be added to the plot. Defaults to c(Inf)
#' @param alpha Select plot transparency. Defaults to .7
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 2.5%
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 97.5%
#' @keywords plot
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   plot_boxplot(x = country,
#'                y = in_sales_v1)
#'
#' aider_data_v1 %>%
#'   plot_boxplot(x = country,
#'                y = in_sales_v1,
#'                fill = country)
#'
#' aider_data_v1 %>%
#'   plot_boxplot(x = country,
#'                y = in_sales_v1,
#'                fill = country,
#'                facet = industry)
#'
#' aider_data_v1 %>%
#'   plot_boxplot(x = country,
#'                y = in_sales_v1,
#'                fill = country,
#'                facet = industry,
#'                ticks = 5,
#'                vline = 500000,
#'                alpha = .7)
#' @export
plot_boxplot <- function(df,
                         x,
                         y,
                         fill = NULL,
                         facet = c(" "),
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         legend = TRUE,
                         vline = c(Inf),
                         alpha = .7,
                         quantile_low = .025,
                         quantile_high = .975
                         ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  get_aider_palletes()

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
    ggtitle(label = ifelse(title == TRUE, glue::glue("Boxplot plot of {rlang::quo_text(var_y)} by {rlang::quo_text(var_x)}"), element_blank())) +
    labs(
      fill = glue::glue("{first_to_upper(rlang::quo_text(var_fill))}:"),
      x = "Value range",
      y = "Density") +
    scale_y_continuous(
      limits = c(
        limits$min,
        limits$max
      ),
      breaks = number_ticks(ticks)
    ) +
    aider_theme() +
    scale_fill_manual(values = wesanderson::wes_palette("Royal1")) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    ) +
    facet_wrap(rlang::quo_text(var_facet))

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

    message("Deam, this graph is amazing!")
    plot +
      geom_boxplot(
        aes_string(
          x = rlang::quo_text(var_x),
          y = rlang::quo_text(var_y),
          fill = rlang::quo_text(var_fill)
        ),
        alpha = alpha
      )
  }
}

# Create a decile plot ---------------------------------------------------

#' Create a decile plot
#'
#' This function creates nicely formatted, standardised decile plots.
#'
#' @param df A data frame
#' @param x A categorical variable for the x axis groupings. Defaults to 'decile'
#' @param y A numerical variable for the y axis levels. Defaults to 'ratio'
#' @param ticks Select the number of ticks on the y axis. Defaults to 10
#' @param angle Select the rotation angle for the x axis labels. Defaults to 0
#' @param title Should the plot title appear automatically. Defaults to TRUE
#' @param legend Should the plot legend appear automatically. Defaults to TRUE
#' @param alpha Select plot transparency. Defaults to .7
#' @param quantile_low Select lower percentile for outliers exclusion. Defaults to 2.5%
#' @param quantile_high Select upper percentile for outliers exclusion. Defaults to 97.5%
#' @keywords plot
#' @examples
#' data("aider_data_v1")
#'
#' aider_data_v1 %>%
#'   calculate_decile_table(binning = sc_v3,
#'                          grouping = target) %>%
#'   plot_deciles()
#' @export
plot_deciles <- function(df,
                         x = decile,
                         y = ratio,
                         ticks = 10,
                         angle = 0,
                         title = TRUE,
                         legend = TRUE,
                         alpha = .7,
                         quantile_low = .025,
                         quantile_high = .975
                         ) {

  if (!is.data.frame(df))
    stop("object must be a data frame")

  get_aider_palletes()

  var_x <- enquo(x)
  var_y <- enquo(y)

  limits_min <- 0
  limits_max <- select(df, !!var_y)[[1]] %>% max() + .05

  message("Wow, what a beautiful graph!")
  plot <- df %>%
    ggplot() +
    geom_bar(
      aes(
        x = decile,
        y = ratio,
        fill = ratio
      ),
      stat = "identity",
      width = .8,
      alpha = alpha
    ) +
    geom_text(
      aes(
        x = decile,
        y = ratio + 0.015,
        label = round(median, 2)
      ),
      position = position_dodge(.9),
      size = 3.2,
      check_overlap = T
    ) +
    ggtitle(label = ifelse(title == TRUE, glue::glue("Decile plot of {rlang::quo_text(var_y)} by {rlang::quo_text(var_x)}"), element_blank())) +
    labs(
      fill = "Ratio",
      x = "Decile",
      y = "Value range") +
    scale_y_continuous(
      limits = c(
        limits_min,
        limits_max
      ),
      labels = scales::percent,
      breaks = number_ticks(ticks)
    ) +
    aider_theme() +
    scale_fill_gradientn(colours = viridisLite::inferno(20, begin = 1, end = 0.55, direction = 1)) +
    theme(
      legend.position = ifelse(legend == TRUE, "bottom", "none"),
      axis.text.x = element_text(angle = angle, hjust = ifelse(angle != 0, 1, .5))
    )
}
