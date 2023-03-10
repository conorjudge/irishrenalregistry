% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_variable_label}
\alias{is_variable_label}
\title{Extract variable name label}
\usage{
is_variable_label(interstroke_data, variable_name)
}
\arguments{
\item{interstroke_data}{An Interstroke Dataset}

\item{variable_name}{A variable name to extract the label for}
}
\value{
An variable label string
}
\description{
Extract variable name label
}
\examples{
is_variable_label(interstroke_data = interstroke_bmi, variable_name = bmi)
}
