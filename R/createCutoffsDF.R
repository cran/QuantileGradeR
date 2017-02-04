#'Create Cutoffs Dataframe
#'
#'\code{createCutoffsDF} is an internal function, which creates a dataframe with
#'identical cutoff values for all ZIP codes (if \code{type = "unadj"}), or
#'quantile cutoffs in a ZIP code (if \code{type = "perc"} or \code{type =
#'"perc.resolve.ties"}). This function is called extensively by the
#'\code{findCutoffs} function.
#'
#'\code{createCutoffsDF} takes in a matrix of restaurants' scores and a vector
#'corresponding to restaurants' ZIP codes, and outputs a data frame of cutoff
#'scores to be used in grade classification. The returned ZIP code cutoff data
#'frame has one row for each unique ZIP code and has \code{(length(gamma)+1)}
#'columns, corresponding to one column for the ZIP code name, and
#'\code{(length(gamma))} cutoff scores separating the \code{(length(gamma)+1)}
#'grading categories.  Across each ZIP code's row, cutoff scores increase and we
#'assume, as in the King County (WA) case, that greater risk is associated with
#'larger inspection scores. (If scores are decreasing in risk, users should
#'transform inspection scores before utilizing functions in the
#'\code{QuantileGradeR} package with a simple function such as \code{f(score) =
#'- score}.)
#'
#'The way in which cutoff scores are calculated for each ZIP code depends on the
#'value of the \code{type} variable.  The \code{type} variable can take one of
#'three values (see later).
#'
#'@section Modes: \code{type = "unadj"} creates a ZIP code cutoff data frame
#'  with the same cutoff scores (meaningful values in a jurisdiction's
#'  inspection system that are contained in the vector \code{gamma}) for all ZIP
#'  codes. This ZIP code data frame can then be used to carry out "unadjusted"
#'  grading, in which a restaurant's most recent routine inspection score is
#'  compared to these cutoffs.
#'@section Modes: \code{type = "perc"} takes in a vector of quantiles,
#'  \code{gamma}, and returns a data frame of the scores in each ZIP code
#'  corresponding to these quantiles (using the "Nearest Rank" definition of
#'  quantile).
#'@section Modes: \code{type = "perc.resolve.ties"} takes in a vector of
#'  quantiles, \code{gamma}, and instead of returning (for B/C cutoffs, for
#'  example) the scores in each ZIP code that result in \emph{at least}
#'  (\code{gamma[2]} x 100)\% of restaurants in the ZIP code scoring less than
#'  or equal to these cutoffs, \code{type = "perc.resolve.ties"} takes into
#'  account the fact that ties exist in ZIP codes. Returned scores for A/B
#'  cutoffs are those that result in the \emph{closest} percentage of
#'  restaurants in the ZIP code scoring less than or equal to the A/B cutoff to
#'  the desired percentage, (\code{gamma[1]} x 100)\%. Similarly, B/C cutoffs
#'  are the scores in the ZIP code that result in the \emph{closest} percentage
#'  of restaurants in the ZIP code scoring less than or equal to the B/C cutoff
#'  and more than the A/B cutoff to the desired percentage, (\code{(gamma[2] -
#'  gamma[1])} x 100)\%.
#'
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
#'@param gamma Numeric vector representing absolute grade cutoffs or quantiles,
#'  depending on \code{type} variable value. Entries in gamma should be
#'  increasing, with \code{gamma[1] <= gamma[2]} etc (this is related to the
#'  "Warning" section and larger scores being associated with higher risk). If
#'  \code{type = "perc"} or \code{type = "perc.resolve.ties"}, gamma values
#'  represent quantiles and should take on values between 0 and 1.
#'@param type Character string that is one of \code{"unadj"},
#'  \code{"perc"}, or \code{"perc.resolve.ties"}, and that indicates the grading
#'  algorithm to be implemented.
#'@keywords internal
#'@export
#'

createCutoffsDF <- function(X, z, gamma, type) {
  ## Preliminary Checks
  #Check X, z, gamma are all of the correct class types; if not, convert.
  X <- matrix(as.numeric(X),nrow = NROW(X))
  z <- as.character(z)
  gamma <- as.numeric(gamma)
  #Check that length of z and number of rows of X match.  If not, throw an error.
  if (NROW(X) != length(z))
    stop("number of rows in X and length of z do not match!")
  #Check that type takes on one of the desired values
  if (!type %in% c("unadj","perc","perc.resolve.ties"))
    stop("type incorrectly specified!")
  #Check that gamma vector is sorted in increasing order
  if(is.unsorted(gamma)){
    stop("gamma vector should be sorted in increasing order!")
  }
  #Check that entries in gamma vector, if type == "perc" or "perc.resolve.ties", are values between 0 and 1
  if ((type == "perc"|type =="perc.resolve.ties") &
      FALSE %in% c(gamma >= 0, gamma <= 1)){
    stop(
      "gamma values represent quantiles and so should be values between 0 and 1!"
    )
  }
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

  ## If type=="unadj", the grading algorithm to be applied uses the same grade
  ## cutoffs for all ZIP codes
  if (type == "unadj") {
    ## First column is column of unique ZIP codes
    zip.cutoffs.df[,"ZIP"] <- unique(z)
    for (i in 1:length(gamma)) {
      zip.cutoffs.df[,c(i + 1)] <- rep(gamma[i], no.unique.zips)
    }
    return(zip.cutoffs.df)
  }

  ## If type=="perc", the grading algorithm uses quantile grade cutoffs
  ## for each ZIP code and gamma is a vector of quantiles
  if (type == "perc") {
    mean.scores.z <- data.frame(mean.scores, z)
    zip.cutoffs.df <-
      stats::aggregate(
        data = mean.scores.z, mean.scores ~ z, FUN = function(mean.scores.vec)
          stats::quantile(
            mean.scores.vec, probs = gamma, type = 1, na.rm = T
          )
      )
    zip.cutoffs.df <- do.call(data.frame, zip.cutoffs.df)
    names(zip.cutoffs.df) <- cutoff.names
    return(zip.cutoffs.df)
  }

  ## If type == "perc.resolve.ties"
  if (type == "perc.resolve.ties") {
    mean.scores.z <- data.frame(mean.scores, z)
    # Distribution of scores in each ZIP code
    distribution.z <-  as.data.frame(table(mean.scores.z))
    # Convert ZIP code column in distribution.z to character type
    distribution.z$z <- as.character(distribution.z$z)
    # Remove scores which no restaurants in ZIP code obtain
    distribution.z <- distribution.z[-which(distribution.z$Freq == 0),]
    # Number of restaurants in each ZIP code
    no.rests.z <- as.data.frame(table(mean.scores.z$z))
    names(no.rests.z) <- c("z", "no.rests")
    distribution.z <- merge(distribution.z, no.rests.z, by = "z")
    # Proportion of restaurants with a given score
    distribution.z$prop.score <- distribution.z$Freq/distribution.z$no.rests
    # Update cutoffs one at a time - finding A/B cutoff first and then finding other cutoffs
    for(i in 1:length(gamma)){
      # A/B Cutoff
      if(i == 1){
        # Desired percentage of restaurants with A grades:
        desired.perc <- gamma[i]
        # Initialize zip.cutoffs.df frame
        zip.cutoffs.df <- as.data.frame(unique(distribution.z$z))
        names(zip.cutoffs.df) <- "ZIP"
      } else{ # B/C cutoffs and beyond
        # Desired percentage of restaurants with "B" grade (in case of i == 2 and B/C cutoff calculation) is difference between current gamma value,
        # which is a quantile, and gamma[i-1]
        desired.perc <- gamma[i] - gamma[i - 1]
      }
      # Current cutoff name
      current.cutoff <- cutoff.names[i + 1]
      # Cumulative sum of proportions
      cumulative.sums <- stats::aggregate(data = distribution.z, prop.score ~ z, FUN = cumsum)
      distribution.z$cum.sum <- as.numeric(unlist(cumulative.sums$prop.score))
      # Absolute difference between cumulative sum of proportions and desired percentage
      distribution.z$diff <- abs(distribution.z$cum.sum - desired.perc)
      # Find minimum absolute difference between cumulative sum of proportions
      # and desired percentage for each restaurant
      min.diffs <- stats::aggregate(data = distribution.z, diff ~ z, FUN = min)
      min.diffs$row.number <- row.names(min.diffs)
      # Cutoff.locs contains the row numbers in distribution.z which correspond to the
      # minimum differences for each ZIP code
      cutoff.locs <- as.data.frame(which(outer(distribution.z$z,
                                               min.diffs$z, "==") & outer(distribution.z$diff,
                                                                          min.diffs$diff, "=="), arr.ind = TRUE))
      cutoff.locs <- merge(cutoff.locs, min.diffs[,-which(colnames(min.diffs) == "diff")],
                           by.x = "col", by.y = "row.number", all.x = TRUE)
      # Are there multiple scores for a ZIP code minimizing difference?
      # If so, choose larger score for cutoff (so that more restaurants receive higher grade in ZIP code)
      cutoff.locs <- stats::aggregate(data = cutoff.locs, row~z, FUN = max)
      cutoff.locs$ZIP <- cutoff.locs$z
      # Identify cutoff values and append to zip.cutoffs.df
      cutoffs <- distribution.z[cutoff.locs$row, c("z", "mean.scores")]
      zip.cutoffs.df <- merge(zip.cutoffs.df, cutoffs, by.x = "ZIP", by.y = "z", all.x = TRUE)
      names(zip.cutoffs.df)[ncol(zip.cutoffs.df)] <- current.cutoff
      # Identify the starting row numbers for each ZIP code in distribution.z
      start.zips <- data.frame(ZIP = unique(distribution.z$z), start.loc = match(unique(distribution.z$z), distribution.z$z))
      # Prepare distribution.z for next for-loop iteration - delete those scores in each ZIP code
      # which now have grades associated with them
      rows.for.deletion <- merge(start.zips,cutoff.locs[,c("row", "ZIP")], by = "ZIP", all = TRUE)
      to.delete <- unlist(lapply(1:nrow(rows.for.deletion), FUN = function(x) rows.for.deletion$start.loc[x]:rows.for.deletion$row[x]))
      distribution.z <- distribution.z[-to.delete,]
    }
    # zip.cutoffs.df to required format
    zip.cutoffs.df$ZIP <- as.character(zip.cutoffs.df$ZIP)
    zip.cutoffs.df[,2:ncol(zip.cutoffs.df)] <- sapply(zip.cutoffs.df[,2:ncol(zip.cutoffs.df)], FUN = function(x)as.numeric(as.character(x)))
    # Replace any NA values with the value of the last non-NA cutoff in row
    na.locations <- which(is.na(zip.cutoffs.df), arr.ind = TRUE)
    if(length(na.locations) > 0){
      na.locations <- as.data.frame(na.locations)
      last.non.na <- stats::aggregate(data = na.locations, col ~ row, FUN = min)
      last.non.na$col <- last.non.na$col - 1
      names(last.non.na) <- c("row", "last.nonna")
      na.locations <- merge(na.locations, last.non.na, by = "row")
      for(i in 1:nrow(na.locations)){
        zip.cutoffs.df[na.locations[i,"row"], na.locations[i,"col"]] <- zip.cutoffs.df[na.locations[i,"row"], na.locations[i,"last.nonna"]]
      }
    }
    # Convert ZIP column to factor type to be consistent with other cases
    zip.cutoffs.df$ZIP <- as.factor(zip.cutoffs.df$ZIP)
    return(zip.cutoffs.df)
  }
}
