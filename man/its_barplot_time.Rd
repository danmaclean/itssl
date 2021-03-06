% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{its_barplot_time}
\alias{its_barplot_time}
\title{generate a barplot}
\usage{
its_barplot_time(
  df,
  names_to = "group",
  values_to = "value",
  colour = "dodgerblue",
  join_tops = FALSE,
  points = FALSE
)
}
\arguments{
\item{names_to}{what to call the variable containing the variable names}

\item{values_to}{what to call the variable containing the values}

\item{colour}{colour of the bars}

\item{join_tops}{add a line joining the tops of bars}

\item{points}{show individual points (geom_jitter)}
}
\value{
ggplot
}
\description{
generate a barplot
}
