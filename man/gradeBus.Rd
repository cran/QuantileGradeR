% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradeBus.R
\name{gradeBus}
\alias{gradeBus}
\title{Grade a Business.}
\usage{
gradeBus(x.bar.i, z.i, zip.cutoffs)
}
\arguments{
\item{x.bar.i}{Numeric inspection score (or mean score) for restaurant in
question.}

\item{z.i}{Character representing ZIP code (or other geographic area) of
business in question.}

\item{zip.cutoffs}{A dataframe with the first column containing ZIP codes and
later columns containing grade cutoff scores for each ZIP code. Cutoff
scores for each ZIP code should be ordered from lowest score in column 2
(representing the cutoff for the best grade) to largest cutoff score in the
final column (representing the cutoff inspection score for the second worst
grade).}
}
\value{
A character representing the grade assigned to the restaurant in
  question ('A', 'B', 'C' etc).
}
\description{
\code{gradeBus} takes in the inspection score for one restaurant, the ZIP
code for the restaurant, a data frame of ZIP
code cutoff information and returns the grade for the business in question.
}
\details{
\code{gradeBus} takes one inspection score for a restaurant (this may be a
mean or the result of a single inspection), the restaurant's ZIP code and a
dataframe of ZIP code cutoffs. It compares each restaurant's inspection score
to cutoff scores in the restaurant's ZIP code.  It finds the smallest cutoff
score in the restaurant's ZIP code that the restaurant's inspection score is
less than or equal to - let's say this is the (\code{letter.index})th cutoff
score - and returns the (\code{letter.index})th letter of the alphabet as the
grade for the restaurant.  \code{gradeBus} is the function called by
\code{\link{gradeAllBus}} in order to grade all businesses.
}
\keyword{internal}

