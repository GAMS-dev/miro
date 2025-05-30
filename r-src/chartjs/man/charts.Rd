% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/charts.R
\name{charts}
\alias{charts}
\alias{cjsBar}
\alias{cjsDoughnut}
\alias{cjsLine}
\alias{cjsPie}
\alias{cjsPolar}
\alias{cjsRadar}
\title{Draw charts}
\usage{
cjsBar(chartjs, labels, stacked = FALSE)

cjsLine(chartjs, labels)

cjsPie(chartjs, labels)

cjsDoughnut(chartjs, labels, cutout = 50)

cjsPolar(chartjs, labels)

cjsRadar(chartjs, labels)
}
\arguments{
\item{chartjs}{a \code{\link{chartjs}} object}

\item{labels}{a vector of characters}

\item{stacked}{a logical, defaults to FALSE. If TRUE, bars will be stacked
at each x value.}

\item{cutout}{the optional percentage of the inner cutout for Doughnut
charts (defaults to 50)}
}
\description{
These functions are used to draw the different types of graphs.
}
