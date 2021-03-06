% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{plot_calibration}
\alias{plot_calibration}
\title{Plot a calibration plot of model performance}
\usage{
plot_calibration(df,
  title = "Lift chart: predicted probabilities vs. actual defaul rates",
  lab_x = "Deciles of predicted probabilities",
  lab_y = "Decile performance")
}
\arguments{
\item{df}{A data frame}

\item{title}{Text that is displayed on as the plot title. Defaults to "Lift chart: evaluation of model predicted probabilities vs. actual defaul rates across deciles"}

\item{lab_x}{Text that is displayed on the x axis. Defaults to "Deciles of predicted probabilities"}

\item{lab_y}{Text that is displayed on the y axis. Defaults to "Decile performance"}
}
\description{
This function creates a nicely formatted, standardised calibration plot. Prior to calling the function
the data should only be in a form of a decile table (calculate_decile_table() function will
do that for you), unless it's already provided.
}
\examples{
df <- tibble::tribble(
  ~decile, ~actual_br, ~predicted_br,
  1,  0.00, 0.01,
  2,  0.00, 0.01,
  3,  0.00, 0.03,
  4,  0.00, 0.05,
  5,  0.30, 0.08,
  6,  0.12, 0.11,
  7,  0.00, 0.16,
  8,  0.21, 0.22,
  9,  0.30, 0.33,
  10, 0.68, 0.59,
)

plot_calibration(df)
}
