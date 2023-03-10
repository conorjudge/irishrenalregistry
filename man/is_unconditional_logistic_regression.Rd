% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_unconditional_logistic_regression}
\alias{is_unconditional_logistic_regression}
\title{Interstroke Unconditonal Logistic Regression}
\usage{
is_unconditional_logistic_regression(interstroke_data, models)
}
\arguments{
\item{interstroke_data}{An Interstroke Dataset}

\item{models}{A list of models for table 2}
}
\value{
A dataframe with model output
}
\description{
Interstroke Unconditonal Logistic Regression
}
\examples{
is_unconditional_logistic_regression(interstroke_data=interstroke_pollution,models=c(case_num~cookplace,case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education,case_num~cookplace+eage+nevfcur+aheiscore+whrs2tert+hhincome+cardiacrfcat+sysbp+education+country+incomectry))
}
