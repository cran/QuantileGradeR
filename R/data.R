#' Example Inspection Scores Matrix.
#'
#' A small dataset of inspection scores.
#'
#' \code{X.kc} contains restaurant inspection information from 11 randomly
#' chosen ZIP codes in the King County (WA) jurisdiction. Establishments and ZIP
#' codes are masked. Inspection information is limited to the 01-01-2012 to
#' 03-25-2016 time period.
#'
#' @format A matrix with 4 columns and ~1500 rows, where each row represents
#'   one business and each column is one inspection cycle.
#'   \code{X.kc[i,j]} represents the inspection score for the
#'   \code{i}th restaurant in the \code{j}th most recent inspection.
"X.kc"


#' Example ZIP Code Vector.
#'
#' A vector of ZIP codes.
#'
#' \code{zips.kc[i]} represents the ZIP code for the restaurant represented in
#' the \code{i}th row of the \code{X.kc} inspection scores matrix. ZIP codes in
#' \code{zips.kc} have the format "zip.j" where j is an integer between 1 and
#' 11, i.e., ZIP codes are masked. In this masking step, we also demonstrate
#' that our functions can be applied not solely over character vectors of real
#' ZIP codes, but any vector of character strings representing the same facet
#' for all restaurants can be used in the grading process.
#'
#' @format A character vector with a length that matches the number of rows of \code{X.kc} (i.e. \code{zips.kc} has
#'   ~1500 elements).  Each entry represents the ZIP code of one business.
"zips.kc"
