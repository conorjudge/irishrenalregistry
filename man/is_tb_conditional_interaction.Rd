% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_tb_conditional_interaction}
\alias{is_tb_conditional_interaction}
\title{is_tb_conditional_interaction A function for adding conditonal analysis P for interaction to Interstroke tables}
\usage{
is_tb_conditional_interaction(
  interstroke_data,
  predictor,
  interaction_variable,
  env = rlang::caller_env()
)
}
\arguments{
\item{interstroke_data}{An Interstroke Dataset}

\item{predictor}{The predictor variable}

\item{interaction_variable}{The interaction variable}
}
\value{
The P for interaction
}
\description{
is_tb_conditional_interaction A function for adding conditonal analysis P for interaction to Interstroke tables
}
\examples{
is_tb_conditional_interaction(interstroke_pollution,polluteh,esex)
}
