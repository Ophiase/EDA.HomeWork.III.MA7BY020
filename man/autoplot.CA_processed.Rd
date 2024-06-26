% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ca_methods.R
\name{autoplot.CA_processed}
\alias{autoplot.CA_processed}
\alias{autoplot.CA}
\title{Autoplot for CA_processed}
\usage{
\method{autoplot}{CA}(x, type = c("all", "scree", "row", "col", "symmetric"), ...)
}
\arguments{
\item{x}{An object of class \code{CA} from the FactoMineR package.}

\item{type}{Type of plot to generate. Options are "all", "scree", "row", "col", "symmetric". Defaults to "all".}

\item{...}{Additional arguments (not used).}
}
\value{
A ggplot object or a combined grid of ggplot objects
}
\description{
Automatically generate plots for CA_processed object
}
