% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_indicator.R
\name{process_indicator}
\alias{process_indicator}
\title{Title}
\usage{
process_indicator(
  data,
  indicator_var_name = NA,
  indicator,
  type = NA,
  units = NA,
  scoring = NA,
  PPTID = NA,
  project_short_title = NA,
  climate = FALSE,
  design_target = FALSE,
  crs = 4326,
  latitude = "latitude",
  longitude = "longitude",
  year = "year",
  other_nest_variables = NA,
  areas = NA,
  areaID = "NAME_E",
  plot_type = "time-series",
  bin_width = 5,
  plot_lm = TRUE,
  plot_lm_se = TRUE
)
}
\arguments{
\item{data}{a data frame or sf data frame that contains the indicator data, the year, and either the latitude and longitude (i.e. data frame) or the geometry (i.e. sf data frame). Other optional columns (e.g. depth) can also be included by naming them in the other_nest_variables parameter.}

\item{indicator_var_name}{character string of the name of the column in the data that contains the indicator data}

\item{indicator}{character string of the name of the indicator}

\item{type}{character string of the instrument/platform/model type of the indicator}

\item{units}{character string of the units of the indicator}

\item{scoring}{character string of the scoring method for the indicator}

\item{PPTID}{numeric project planning tool project ID}

\item{project_short_title}{character string of a short title for the project}

\item{climate}{logical indicating whether the indicator is a climate indicator}

\item{design_target}{logical indicating whether the indicator is a design target}

\item{crs}{coordinate reference system of the data}

\item{latitude}{character string of the name of the column in the data that contains the latitude}

\item{longitude}{character string of the name of the column in the data that contains the longitude}

\item{year}{character string of the name of the column in the data that contains the year}

\item{other_nest_variables}{character vector of the names of other columns in the data that should be nested}

\item{areas}{an sf data frame that contains the areas to which the data should be joined (i.e. MPAs)}

\item{areaID}{character string of the name of the column in the areas data that contains the area ID (i.e. NAME_E by default)}

\item{plot_type}{character string of the type of plot to generate (e.g. time-series, boxplot, violin)}

\item{bin_width}{numeric width of the bins for the boxplot or violin plot}

\item{plot_lm}{logical indicating whether to plot a linear model on the plot}

\item{plot_lm_se}{logical indicating whether to plot the standard error of the linear model on the plot}
}
\description{
Title
}
