#'Find Cutoff Values.
#'
#'\code{findCutoffs} applies a quantile adjustment to inspection scores within a
#'jurisdiction's subunits (e.g. ZIP codes) and creates a data frame of cutoff
#'values to be used for grading restaurants or other inspected entities.
#'
#'In our documentation, we use the language "ZIP code" and "restaurant",
#'however, our grading algorithm and our code can be applied to grade other
#'inspected entities; and quantile cutoffs can be sought in subunits of a
#'jurisdiction that are not ZIP codes. For example, it may make sense to search
#'for quantile cutoffs in an inspector's allocated inspection area or within a
#'census tract. We chose to work with ZIP codes in our work because area
#'assignments for inspectors in King County (WA) tend to be single or multiple
#'ZIP codes, and we desired to assign grades based on how a restaurant's scores
#'compare to other restaurants assessed by the same inspector.  We could have
#'calculated quantile cutoffs in an inspector's allocated area, but inspector
#'areas are not always contiguous. Because food choices are generally local, ZIP
#'codes offer a transparent and meaningful basis for consumers to distinguish
#'establishments. Where "ZIP code" is referenced, please read "ZIP code or other
#'subunit of a jurisdiction" and "restaurant" should read "restaurant or other
#'entity to be graded".
#'
#'
#'\code{findCutoffs} takes in a vector of cutoff scores, \code{gamma}, a matrix
#'of restaurants' scores, \code{X}, and a vector corresponding to restaurants'
#'ZIP codes, \code{z}, and outputs a data frame of cutoff scores to be used in
#'the \code{\link{gradeAllBus}} function to assign grades to restaurants.
#'\code{findCutoffs} first carries out "unadjusted grading" and compares
#'restaurants' most recent routine inspection scores to the raw cutoff scores
#'contained in \code{gamma} and assigns initial grades to restaurants. Grade
#'proportions in this scheme are then used as initial quantiles to find quantile
#'cutoffs in each ZIP code (or quantile cutoffs accommodating for the presence
#'of score ties in the ZIP code, depending on the value of \code{resolve.ties};
#'see the Modes section). Restaurants are then graded with the ZIP code quantile
#'cutoffs, and grading proportions are compared with grading proportions from
#'the unadjusted system. Quantiles are iterated over one at a time (by the
#'internal \code{percentileSeek} function, which uses a binary search root
#'finding method) until grading proportions with ZIP code quantile cutoffs are
#'within a certain tolerance (as determined by \code{restaurant.tol}) of the
#'unadjusted grading proportions. This iterative step is important because of the
#'discrete nature of the inspection score distribution, and the existence of
#'large numbers of restaurants with the same inspection scores.
#'
#'
#'The returned ZIP code cutoff data frame has one row for each unique ZIP code
#'and has \code{(length(gamma)+1)} columns, corresponding to one column for the
#'ZIP code name, and \code{(length(gamma))} cutoff scores separating the
#'\code{(length(gamma)+1)} grading categories.  Across each ZIP code's row,
#'cutoff scores increase and we assume, as in the King County (WA) case, that
#'greater risk is associated with larger inspection scores. (If scores are
#'decreasing in risk, users should transform inspection scores  with a simple
#'function such as \code{f(score) = - score} before using any of the functions
#'in \code{QuantileGradeR}.)
#'
#'
#' @section Modes: When \code{resolve.ties = TRUE}, in order to calculate
#'   quantile cutoffs in a ZIP code, we alter the definition of quantile from
#'   the usual "Nearest Rank" definition and use the "Quantile Adjustment (with
#'   Ties Resolution)" definition that is discussed in Appendix J of
#'   Ho, D.E., Ashwood, Z.C., and Elias, B. "Improving the Reliability of Food
#'   Safety Disclosure: A Quantile Adjusted Restaurant Grading System for
#'   Seattle-King County" (working paper). In particular, once we have found the
#'   optimal set of quantiles to be applied across ZIP codes, \code{p},
#'   with the \code{percentileSeek} function, instead of returning (for B/C
#'   cutoffs, for example) the scores in each ZIP code that result in \emph{at
#'   least} (\code{p[2]} x 100)\% of restaurants in the ZIP code scoring
#'   less than or equal to these cutoffs, the mode \code{resolve.ties = TRUE}
#'   takes into account the ties that exist in ZIP codes. Returned scores for
#'   A/B cutoffs are those that result in the \emph{closest} percentage of
#'   restaurants in the ZIP code scoring less than or equal to the A/B cutoff to
#'   the desired percentage, (\code{p[1]} x 100)\%. Similarly, B/C cutoffs
#'   are the scores in the ZIP code that result in the \emph{closest} percentage
#'   of restaurants in the ZIP code scoring less than or equal to the B/C cutoff
#'   and more than the A/B cutoff to the desired percentage, (\code{(p[2] -
#'   p[1])} x 100)\%.
#'
#' @section Modes: When \code{resolve.ties = FALSE}, we use the usual "Nearest
#'   Rank" definition of quantile when applying the optimal quantiles,
#'   \code{p}, across ZIP codes.
#'
#'
#'@section Warning: \code{findCutoffs} will produce cutoff scores even for ZIP
#'  codes with only one restaurant: situations in which a quantile adjustment
#'  shouldn't be used. It is the job of the user to ensure that, if using the
#'  \code{findCutoffs} function, it makes sense to do so.  This may involve only
#'  performing the quantile adjustment on larger ZIP codes and providing
#'  absolute cutoff points for smaller ZIP codes, or may involve aggregating
#'  smaller ZIP codes into a larger geographical unit and then performing the
#'  quantile adjustment on the larger area (the latter approach is the one we
#'  adopted).
#'@section Warning: As mentioned previously, \code{findCutoffs} was created for
#'  an inspection system that associates greater risk with larger inspection
#'  scores. If the inspection system of interest associates greater risk with
#'  reduced scores, it will be neccessary to perform a transformation of the
#'  scores matrix before utilizing the \code{findCutoffs} function. However a
#'  simple function such as \code{f(score) = - score} would perform the
#'  necessary transformation.
#'
#'
#'
#'@param X Numeric matrix of size \code{n} x \code{p}, where \code{n} is the
#'  number is restaurants to be graded and \code{p} is the number of inspections
#'  to be used in grade assignment.  Entry  \code{X[i,j]} represents the
#'  inspection score for the \code{i}th restaurant in the \code{j}th most recent
#'  inspection.
#'@param z Character vector of length \code{n} representing ZIP codes (or other
#'  subunits within a jurisdiction).  \code{z[i]} is the ZIP code corresponding
#'  to the restaurant with inspection scores in row \code{i} of \code{X}.
#'@param gamma Numeric vector representing absolute grade cutoffs. Entries in
#'  gamma should be increasing, with \code{gamma[1] <= gamma[2]} etc (this is
#'  related to the "Warning" section and larger scores being associated with
#'  higher risk).
#'@param resolve.ties Boolean value that determines the definition of quantile
#'  to be used after optimal quantiles have been found with the
#'  \code{percentileSeek} function. See Modes below, as well as Appendix J of
#'  Ho, D.E., Ashwood, Z.C., and Elias, B. "Improving the Reliability of Food
#'  Safety Disclosure: A Quantile Adjusted Restaurant Grading System for
#'  Seattle-King County".
#'@param restaurant.tol An integer indicating the maximum difference in the number of
#'  restaurants in a grading category between the unadjusted and adjusted
#'  grading algorithms (for the top \code{length(gamma)} grading categories).
#'@param max.iterations The maximum number of iterations that the iterative
#'  algorithm (carried out by the internal \code{percentileSeek} function)
#'  should run in order to find optimal quantiles for ZIP cutoffs. The iterative
#'  algorithm is described in more detail below.
#'
#' @examples
#'
#' ## ==== Quantile-Adjusted Grading =====
#' ## ZIP Code Cutoffs
#' # In King County, meaningful scores in the inspection system are 0 and 30:
#' # more than 50% of restaurants score 0 points in a single inspection round,
#' # and 30 is the highest score that a restaurant can be assigned before it is
#' # subject to a return inspection, hence these values form our gamma vector.
#' # The output dataframe, zipcode.cutoffs.df, has ten rows and three columns: one
#' # row for every unique ZIP code in zips.kc, one column for the ZIP name, the
#' # second column for the A/B cutoff (Gamma.A) and the third column for the B/C
#' # cutoff (Gamma.B).
#'
#'  zipcode.cutoffs.df <- findCutoffs(X.kc, zips.kc, gamma = c(0, 30))
#'
#' ## ==== Traditional Grading Systems ====
#' ## ZIP Code Cutoffs
#' # Traditional (unadjusted) restaurant grading systems use the same cutoff scores
#' # for all ZIP codes. To allow comparison, an unadjusted ZIP code cutoff frame
#' # for King County is generated by the internal createCutoffsDF function:
#'
#'  unadj.cutoffs.df <- createCutoffsDF(X.kc, zips.kc, gamma = c(0, 30), type = "unadj")
#'@export
#'


findCutoffs <- function(X, z, gamma, resolve.ties = TRUE, restaurant.tol = 10, max.iterations = 20){
  ## Preliminary Checks
  #Check X, z, gamma are all of the correct class types; if not, convert.
  X <- matrix(as.numeric(X),nrow = NROW(X))
  z <- as.character(z)
  gamma <- as.numeric(gamma)
  #Check that length of z and number of rows of X match.  If not, throw an error.
  if (NROW(X) != length(z))
    stop("number of rows in X and length of z do not match!")
  #Check that gamma vector is sorted in increasing order
  if(is.unsorted(gamma)){
    stop("gamma vector should be sorted in increasing order!")
  }
  #Check if any of the ZIP codes have less than, say 10 restaurants, in which
  #case a quantile adjustment may not be the most appropriate grading system
  if (TRUE %in% (data.frame(table(z))$Freq < 10))
    warning(
      "at least one ZIP code has less than 10 restaurants. A quantile adjustment in this case may not be the most appropriate form of grading.", call. = FALSE
    )
  ## Initialize zip.cutoffs.df data frame - the object to be returned with a
  ## row for each unique ZIP code and the number of columns equal to the
  ## number of grades (one column for ZIPs, the remainder of columns for the
  ## (no.grades-1) ZIP code cutoff points)
  no.grades <- length(gamma) + 1
  no.unique.zips <- length(unique(z))
  zip.cutoffs.df <-
    data.frame(matrix(NA, nrow = no.unique.zips, ncol = no.grades))
  ## Names for columns in zip.cutoffs.df
  cutoff.names <- paste("Gamma", LETTERS[1:(no.grades - 1)], sep = ".")
  cutoff.names <- append(cutoff.names, "ZIP", after = 0)
  names(zip.cutoffs.df) <- cutoff.names

  mean.scores <- rowMeans(X, na.rm = T)

  ## The grading algorithm to be applied uses absoulte grade cutoffs and finds
  ## the proportions of each grade in the unadjusted grading scheme.  It then
  ## applies the percentileSeek function in order to find the relevant quantiles
  ## to be applied to ZIP codes in order that the adjusted system proportions
  ## match the unadjusted proportions.
    ## Check how the restaurant.tol compares to the number of restaurants to be graded.
    unadj.cutoffs <- createCutoffsDF(X, z, gamma, type = "unadj")
    ## Unadjusted grading compares most recent inspection scores to uniform absolute cutoff values
    unadj.grades <-
      gradeAllBus(scores = X[,c(1)], z, zip.cutoffs = unadj.cutoffs)
    unadj.grades <- factor(unadj.grades, levels = LETTERS[1:no.grades])
    grade.proportions <-
      as.data.frame(table(unadj.grades) / length(unadj.grades[which(!is.na(unadj.grades))]))
    if (nrow(grade.proportions) == 0)
      stop("grade proportions data frame for unadjusted grading has 0 rows")
    names(grade.proportions) <- c("grade", "proportion")
    desired.props <- grade.proportions$proportion
    ## Run percentileSeek function in order to find the quantiles that should be applied across all ZIP codes in order to match uniform absolute proportions
    gamma.updated <-
      percentileSeek(
        mean.scores, z, desired.props, restaurant.tol = restaurant.tol, max.iterations = max.iterations, resolve.ties = resolve.ties
      )
    #print(paste("final gamma vector =", gamma.updated))
    ## Run the createCutoffsDF function with the new quantiles
    if(resolve.ties == TRUE){
      zip.cutoffs.df <-
        createCutoffsDF(X, z, gamma.updated, type = "perc.resolve.ties")
    } else{
      zip.cutoffs.df <-
        createCutoffsDF(X, z, gamma.updated, type = "perc")
    }
    return(zip.cutoffs.df)

}













