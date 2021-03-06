% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gradeBus.R
\name{gradeAllBus}
\alias{gradeAllBus}
\title{Grade Businesses.}
\usage{
gradeAllBus(scores, z, zip.cutoffs)
}
\arguments{
\item{scores}{Numeric vector of length \code{n}, where \code{n} is the number is
restaurants to be graded.  Each entry is the inspection score for one
business.}

\item{z}{Character vector of length \code{n}, where each entry is the ZIP
code (or other geographic area) of a business.  The order of businesses in
\code{z} is the same as the order of businesses in \code{scores}.}

\item{zip.cutoffs}{A dataframe with the first column containing all of the
ZIP codes in z and later columns containing cutoff scores for each ZIP code
for grade classification.  Cutoff scores for each ZIP code should be
ordered from lowest score in column 2 (representing the cutoff for the best
grade) to the largest cutoff score in the final column (representing the
cutoff inspection score for the second worst grade). This dataframe will
most likely have been generated by the \code{findCutoffs} function.}
}
\value{
A character vector of length n, with each entry corresponding to the
  grade that the restaurant received.
}
\description{
\code{gradeAllBus} takes in a vector of business inspection scores, business
ZIP codes and a data frame of ZIP code cutoff scores (generated by the
\code{findCutoffs} function) and returns a vector of business grades.
}
\details{
As explained in the \code{findCutoffs} documentation, we use the language "ZIP
code" and "restaurant", however, our grading algorithm can be applied to grade
other inspected entities. As with \code{findCutoffs}, where "ZIP code" is
referenced, please read "ZIP code or other subunit of a jurisdiction" and
"restaurant" should read "restaurant or other entity to be graded".

\code{gradeAllBus} takes a vector of inspection scores (one score for each
restaurant: the score can be a mean across multiple inspections or the result
of a single inspection), a vector of ZIP codes and a dataframe of ZIP code
cutoffs (most likely generated by the \code{findCutoffs} function).  It
compares each restaurant's inspection score to cutoff scores in the
restaurant's ZIP code. It finds the smallest cutoff score in the restaurant's
ZIP code that the restaurant's inspection score is less than or equal to -
let's say this is the (\code{letter.index})th cutoff score - and returns the
(\code{letter.index})th letter of the alphabet as the grade for the
restaurant. The returned vector of grades maintains the order of businesses
in vector inputs \code{scores} and in \code{z}).
}
\examples{


## ===== Quantile-Adjusted Grading =====
## ZIP Code Cutoffs (see findCutoffs documentation for an explanation of how
## these are calculated)

 zipcode.cutoffs.df <- findCutoffs(X.kc, zips.kc, gamma = c(0, 30))

## In King County, we use a restaurant's mean inspection score over the last
## four inspections for grading (see Ho, D.E.,
## Ashwood, Z.C., and Elias, B. "Improving the Reliability of Food Safety
## Disclosure: A Quantile Adjusted Restaurant Grading System for Seattle-King
## County" (working paper)). Calculate these mean scores:

 mean.scores <- rowMeans(X.kc, na.rm = TRUE)

## We then use the mean scores and the zipcode.cutoffs.df dataframe to perform
## grading:

 adj.grades <- gradeAllBus(mean.scores, zips.kc, zipcode.cutoffs.df)


## ===== Traditional Grading Systems =====
## For comparison, calculate grades as if we had used a traditional grading
## system in King County, with 0 and 30 as the A/B and B/C cutoffs for all ZIP
## codes.

## Cutoffs:

 unadj.cutoffs.df <- createCutoffsDF(X.kc, zips.kc, gamma = c(0, 30), type = "unadj")

## Grades (traditional grading systems only use the most recent inspection score
## for grading):

 unadj.grades <- gradeAllBus(scores = X.kc[,c(1)], zips.kc, zip.cutoffs = unadj.cutoffs.df)


## ===== Comparison: Quantile-Adjusted Grading and Traditional Grading ===
## Proportion of restaurants in each grading category varies dramatically
## between ZIPs in traditional compared to quantile-adjusted grading; these
## differences do not reflect sanitation differences, but rather differences in
## stringency across inpectors (see: Ho, D.E., Ashwood, Z.C., and Elias, B.
## "Improving the Reliability of Food Safety Disclosure: A Quantile Adjusted
## Restaurant Grading System for Seattle-King County" (working paper)).
## Tabulate restaurants in each ZIP code in each grading category and then
## divide by total number of restaurants in each ZIP to obtain proportions.
## Proportions are rounded to 2 decimal places.

## Traditional Grading

 foo1 <- round(table(zips.kc, unadj.grades)/apply(table(unadj.grades, zips.kc), 2, sum), 2)

## Quantile-Adjusted Grading

 foo2 <- round(table(zips.kc, adj.grades)/apply(table(adj.grades, zips.kc), 2, sum), 2)

}

