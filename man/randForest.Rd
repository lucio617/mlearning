% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{randForest}
\alias{randForest}
\title{Random Forest}
\usage{
randForest(t, d)
}
\arguments{
\item{t}{Target Variable}

\item{d}{Dataset}
}
\value{

}
\description{
Performs Random Forest Classification algorithm for the target variable on the basis of given dataset
Splits the given dataset into training and testing in the ratio of 80:20 respectively
Also generates a confusion matrix toe predict the error of the model
}
\note{
Pass the target value with '$' .
Ex- dataset$target(d$t)
}
\examples{
randomForest(t,d)
}
