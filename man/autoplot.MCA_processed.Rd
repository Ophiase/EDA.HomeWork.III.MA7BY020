% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mca_methods.R
\name{autoplot.MCA_processed}
\alias{autoplot.MCA_processed}
\alias{autoplot.MCA}
\title{Autoplot for MCA_processed}
\usage{
\method{autoplot}{MCA}(x, type = c("all", "scree", "row", "col", "symmetric"), ...)
}
\arguments{
\item{x}{An object of class \code{MCA} from the FactoMineR package.}

\item{type}{Type of plot to generate. Options are "all", "scree", "row", "col", "symmetric". Defaults to "all".}

\item{...}{Additional arguments (not used).}
}
\value{
A ggplot object or a combined grid of ggplot objects
}
\description{
Automatically generate plots for MCA_processed object
}
