#'Find percentile values (to match a set of global proportions).
#'
#'\code{percentileSeek} returns a set of percentiles to be applied across
#'subunits (e.g. ZIP codes) of a larger area (e.g. a jurisdiction), so as to
#'rank items within each subunit (e.g. restaurants) and group these items into
#'grade categories. \code{percentileSeek} allows the user to set the desired
#'global proportion of items in each grade category.
#'
#'In our documentation, we use the language ``ZIP code'' and ``restaurant'',
#'however, our algorithms and code can be applied much more broadly to other
#'inspected or scored entities; and percentile cutoffs can be sought in subunits
#'(of a larger area) that are not ZIP codes. Where ``ZIP code'' is referenced,
#'please read ``ZIP code or other subunit of a larger area'' and ``restaurant''
#'should read ``restaurant or other entity to be graded''.
#'
#'\code{percentileSeek} was designed for situations in which a significant
#'number of ties in the scores of items within subunits (e.g. ties in restaurant
#'inspection scores in ZIP codes) result in the obvious choice of percentiles
#'(namely those obtained from the desired proportions) not yielding the desired
#'proportions globally. \code{percentileSeek} will iterate over different values
#'for the first percentile (using the update process described in the
#'\code{\link{updateGamma}} documentation) until the proportion of (gradeable)
#'restaurants scoring ``A'' grades (when ZIP cutoffs are percentile values) is
#'within \code{(restaurant.tol/ no.gradeable.rests)} of the desired proportion
#'of As, where \code{no.gradeable.rests} is the number of gradeable restaurants,
#'and gradeable restaurants are those that have both ZIP code and inspection
#'score information. The algorithm will then seek to find a larger percentile to
#'match the proportion of gradeable restaurants scoring ``B'' grades with the
#'desired proportion of Bs and so on, until the proportions of restaurants
#'gaining the top \code{(lengh(desired.props) - 1)} grades are within the required
#'tolerance of their desired proportions. Note: there is thus no requirement
#'that the proportion of restaurants gaining the worst grade matches the desired
#'proportion for worst grade - these can be quite different (depending on the
#'number of restaurants being graded and the number of grade categories) and no
#'error will be reported.
#'
#'Of course, \code{percentileSeek} can only find a solution if one exists.  It
#'could be the case that it is simply not possible with a particular set of
#'scores to match the desired proportions. We have included some failsafes to
#'catch some of the simplest instances in which no solution will exist. For instance, one
#'possible reason for failure is selecting a desired proportion of ``A'' grades that
#'is below the global minimum proportion of ``A''s.  Totaling the number of
#'restaurants with the best inspection scores in their ZIP codes and dividing by
#'the number of gradeable restaurants provides the global minimum proportion of
#'``A''s. Running \code{percentileSeek} can be a useful way to test whether a
#'solution is likely to exist. If reported results of the \code{percentileSeek}
#'function are outwith the standard [0, 1] interval for percentiles, or if the
#'number of iterations exceeds the maximum number of iterations, this could be
#'indicative that no solution exists.
#'
#'
#'An example of when the \code{percentileSeek} function could be used outside
#'of the restaurant context is if you were tasked with finding the top 3 percent
#'of students in a state.  We know that each school has its own GPA system and
#'so comparing students by raw GPA does not make sense. We could thus desire to
#'perform a percentile adjustment at each school and select the top 3 percent of
#'students at each school. Unfortunately, some schools do not utilize the full
#'spectrum of GPA scores available and so it may be the case that the top 5
#'percent of students at school 1 have the same GPA and cannot be
#'distinguished from one another. Using \code{percentileSeek} with each
#'restaurant replaced by a student, each restaurant's inspection score replaced
#'by the student's GPA and each ZIP code replaced by a school, we could
#'investigate whether it is possible to satisfy the 3 percent globally desired
#'proportion.  \code{percentileSeek} would reduce the percentile applied across
#'schools (from the initial 3 percent), which would still select the 5 percent
#'of students at school 1 for nomination, but would try to take advantage of
#'the fact that some schools do use more of their GPA scale. Of course, issues
#'of fairness do arise and one wonders why school 2, which distinguishes its
#'students better than school 1, should have fewer students represented in the
#'globally selected 3 percent. We only advocate the use of \code{percentileSeek}
#'for situations in which there is good reason to demand certain global
#'proportions. In the school selection case, this may be that there are only
#'finite resources available to be given to the top 3 percent of students and it is
#'simply not possible to extend these resources to the top 3 percent of students
#'at each school. In the restaurant case, we desire to select the top
#'restaurants in each ZIP code to be assigned an 'A' grade; however we also do
#'not want to design a grading system that is seen to inflate grades compared to
#'an unadjusted grading system (one based on absolute uniform grade cutoffs
#'across the whole jurisdiction).
#'
#'
#'@param scores Numeric vector of size \code{n}, where \code{n} is the number is restaurants
#'  to be graded. \code{scores[i]} represents the mean or raw
#'  inspection score for restaurant \code{i}.
#'@param z Character vector representing ZIP codes. \code{z[i]} is the ZIP code for
#'  restaurant \code{i}.
#'@param desired.props Numeric vector representing desired global grade
#'  proportions across the entire jurisdiction. \code{desired.props[j]} is the desired
#'  proportion of total (gradeable) restaurants in the \code{j}th highest
#'  grading category.
#'@param restaurant.tol Integer value representing the maximum difference in
#'  number of restaurants suggested by \code{desired.props} and the actual
#'  number of restaurants in each of the top \code{(length(desired.props) - 1)}
#'  grade categories.
#'@param max.iterations Integer value specifying the maximum number of
#'  calls of the \code{\link{updateGamma}} percentile update function
#'  for each of the sought after percentiles.
#'@param resolve.ties Boolean value specifying interpretation of how the
#'  function's returned percentiles will be applied across subunits. Should
#'  \emph{as close to} (desired.props[1])\% of restaurants in a ZIP code receive
#'  an "A" grade, and \emph{as close to} (desired.props[2])\% of restaurants in a
#'  ZIP code receive "B" grades (\code{resolve.ties = TRUE} case)? Or should the
#'  returned percentiles be interpretted as R \link[stats]{quantile} \code{Type = 1}
#'  percentiles, and \emph{at least} (desired.props[1])\% of restaurants in a ZIP
#'  code receive "A" grades?
#'@return A numeric vector with the percentiles to be applied to each ZIP code
#'  so as to achieve the desired proportion of grades.
#'@keywords internal
#'@export
#'
  percentileSeek <- function(scores, z, desired.props, restaurant.tol = 10, max.iterations = 20, resolve.ties = FALSE) {
    #==========
    # Set-up
    scores <- as.numeric(scores)
    z <- as.character(z)
    desired.props <- as.numeric(desired.props)
    # Set Up
    no.grades <- length(desired.props)
    # Check that desired.props add to 1:
    if(sum(desired.props) <= 0.999 | sum(desired.props) >= 1.001) stop("sum of desired proportions should equal 1!")
    #==========
    # Preliminary Checks:
    # Is it possible to reach the globally desired proportion for the best
    # grade? One possible reason for failure is that there is a minimum global
    # proportion of As - and no percentile choice can reduce the global
    # proportion to meet a desired proportion value below this minimum.
    # We can check for this source of failure as follows:
    # Find the best inspection score awarded in each ZIP code and then sum, across ZIP
    # codes, to find the total number of restaurants with the best scores in
    # their ZIP codes. If the globally desired proportion for the best grade,
    # desired.prop[1], is less than the proportion of restaurants that score the
    # best grade in their ZIP code, it is not possible to reduce percentiles to
    # match desired.prop[1] and so the code will stop before the iterative
    # process begins
    score.zip.df <- data.frame(z, scores)
    # table scores and ZIP codes
    zip.tab.scores <- as.data.frame(table(score.zip.df$scores, score.zip.df$z))
    names(zip.tab.scores) <- c("score", "zip", "no.rests")
    # remove entries with 0 restaurants
    zip.tab.scores <- zip.tab.scores[which(zip.tab.scores$no.rests > 0),]
    # Order by ZIP code and by increasing score in each ZIP code
    zip.tab.scores <- zip.tab.scores[order(zip.tab.scores$zip, zip.tab.scores$score, decreasing=F), ]
    # Find the first instance in which each ZIP code appears - this will give us
    # the minimum score in each ZIP code
    zip.min.scores <- zip.tab.scores[match(unique(zip.tab.scores$zip), zip.tab.scores$zip),]
    # Total the restaurants with the best scores in each ZIP code
    # Divide this by the total number of restaurants (that are gradeable) in order to find the minimum proportion of
    # restaurants with the best grade
    # Gradeable restaurants are those with ZIP code information and with an inspection score
    gradeable.rests <- score.zip.df[which(stats::complete.cases(score.zip.df)),]
    min.prop.best <- sum(zip.min.scores$no.rests)/nrow(gradeable.rests)
    if(desired.props[1] < min.prop.best) stop(paste("minimum global proportion of As is ", min.prop.best))
    #===========
    # Another obvious reason that no solution may exist: if it is possible only
    # to distinguish between a maximum of "max.unique.zip" scores in a ZIP code and the number
    # of desired grades is more than "max.unique.zip", then it will not be possible to match
    # the desired proportions in that case.
    unique.scores.zip <- stats::aggregate(data = score.zip.df, scores ~ z, FUN = unique)
    no.unique.zip <- as.numeric(lapply(unique.scores.zip$scores, FUN = length))
    max.unique.zip <- max(no.unique.zip)
    if(no.grades > max.unique.zip) stop(paste("Number of grades exceeds maximum number of distinguishable scores in a ZIP code, which is ", max.unique.zip))
    #===========
    # Initial percentiles gamma.perc
    gamma.perc<-as.numeric()
    for(j in 1:(length(desired.props)-1)){
      gamma.perc[j]<- sum(desired.props[1:j])
    }
    #============
    # Update gamma.perc
    for(j in 1:(length(desired.props)-1)){
      gamma.perc[j]<- updateGamma(scores, z, desired.props, gamma.perc, index.to.update = j, restaurant.tol = restaurant.tol, max.iterations = max.iterations, resolve.ties = resolve.ties)
    }
    #============
    # Check - returned percentile values should be values between 0 and 1
    if(FALSE %in% c(gamma.perc >= 0, gamma.perc <= 1))stop("updateGamma method has failed and returned percentiles are not between 0 and 1. It is likely that a solution does not exist and adjusted proportions cannot be matched to desired proportions.")

    return(gamma.perc)
  }


  #' Update Gamma (percentile value).
  #'
  #'\code{updateGamma} is the percentile update function called by
  #'\code{\link{percentileSeek}} as \code{\link{percentileSeek}} attempts to
  #'match a set of grade proportions to a set of \code{desired.props}.
  #'
  #'\code{updateGamma} performs the update of
  #'\code{gamma.perc[index.to.update]}. In particular, \code{gamma.perc[index.to.update]} will
  #'be updated until either the number of updates has reached
  #'\code{max.iterations}, or the difference between the proportion of
  #'(gradeable) restaurants scoring the \code{(index.to.update)}th highest grade
  #'is within \code{(restaurant.tol/ no.gradeable.rests)} of the desired
  #'proportion, where \code{no.gradeable.rests} is the number of gradeable
  #'restaurants (restaurants that have both ZIP code and inspection score
  #'information).  Initially, \code{gamma.perc[index.to.update]} is updated
  #'according to the rule \code{gamma.perc[index.to.update] <-
  #'(gamma.perc[index.to.update] - diff.aj.desired)}, where \code{diff.aj.desired}
  #'is the difference between the actual proportion of restaurants assigned the grade
  #'of interest and the desired proportion. However, if the algorithm locates
  #'values of \code{gamma.perc[index.to.update]} that produce grade proportions
  #'that are both higher and lower than the desired proportion,
  #'\code{gamma_upper} and \code{gamma_lower} respectively, the update rule
  #'becomes \code{gamma.perc[index.to.update]<- 0.5*(gamma_upper +
  #'gamma_lower)}, as in the bisection root finding method.
  #'
  #'@param scores Numeric vector of size \code{n}, where \code{n} is the number is restaurants
  #'  to be graded. \code{scores[i]} represents the mean or raw
  #'  inspection score for restaurant \code{i}.
  #'@param z Character vector representing ZIP codes. \code{z[i]} is the ZIP code for
  #'  restaurant \code{i}.
  #'@param desired.props Numeric vector representing desired global grade
  #'  proportions across the entire jurisdiction. \code{desired.props[j]} is the desired
  #'  proportion of total (gradeable) restaurants in the \code{j}th highest
  #'  grading category.
  #'@param gamma.perc Numeric vector representing an initial set of percentiles.
  #'@param index.to.update Integer value in the set
  #'  \code{1:(length(desired.props)-1)} that represents the particular
  #'  percentile to be updated in the current run of \code{updateGamma}.
  #'  (Percentiles are not updated simultaneously, but rather are updated
  #'  sequentially with the smallest percentiles being the first to be updated.)
  #'@param restaurant.tol Integer value representing the maximum difference in
  #'  number of restaurants suggested by \code{desired.props} and the actual
  #'  number of restaurants in each of the top \code{(length(desired.props) -
  #'  1)} grade categories.
  #'@param iter Integer value representing the current iteration of \code{updateGamma}.
  #'@param max.iterations Integer value specifying the maximum number of
  #'  calls of the \code{\link{updateGamma}} percentile update function
  #'  for each of the sought after percentiles.
  #'@param gamma_upper Numeric or NA value representing a value of
  #'  \code{gamma.perc[index.to.update]} that results in too many restaurants
  #'  gaining the desired grade proportion.
  #'@param gamma_lower Numeric or NA value representing a value of
  #'  \code{gamma.perc[index.to.update]} that results in too few restaurants
  #'  gaining the desired grade proportion.
  #'@param resolve.ties Boolean value specifying interpretation of how the
  #'  function's returned percentile will be applied across the subunits see:
  #'  \link{percentileSeek}. Should \emph{as close to} (desired.props[1])\% of
  #'  restaurants in a ZIP code receive an "A" grade, and \emph{as close to}
  #'  (desired.props[2])\% of restaurants in a ZIP code receive "B" grades
  #'  (\code{resolve.ties = TRUE} case)? Or should the returned percentiles be
  #'  interpretted as R \link[stats]{quantile} \code{Type = 1} percentiles, and
  #'  \emph{at least} (desired.props[1])\% of restaurants in a ZIP code receive
  #'  an "A" grade?
  #'@return A numeric value representing a percentile to be applied to each ZIP code
  #'  so as to achieve a particular desired proportion of grades.

  #'@keywords internal
  #'@export
  #'

  updateGamma <- function(scores, z, desired.props, gamma.perc, index.to.update,
                          restaurant.tol = 10, iter = 1, max.iterations = 20,
                          gamma_upper = NA, gamma_lower = NA,
                          resolve.ties = FALSE) {
    # Stop if maximum number of iterations is reached
    if (iter > max.iterations) stop("maximum number of iterations reached in gamma update.  Consider increasing max.iterations, or increasing restaurant.tol. It is possible that no solution exists and that adjusted grade proportions cannot be matched with desired grade proportions.")
    # Grade using percentiles gamma.perc vector
    if(resolve.ties == TRUE){
      zip.cutoffs <-
        createCutoffsDF(scores, z, gamma = gamma.perc, type = "perc.resolve.ties")
    } else{
      zip.cutoffs <- createCutoffsDF(scores, z, gamma = gamma.perc, type = "perc")
    }
    adj.grades <- gradeAllBus(scores, z, zip.cutoffs)
    adj.grades <- factor(adj.grades, levels=LETTERS[1:length(desired.props)])
    # Count number of restaurants graded - restaurants are not graded if they do not have inspection score information, or are lacking ZIP code information
    no.rests.graded <- length(adj.grades[which(!is.na(adj.grades))])
    adj.gr.props <- as.data.frame(table(adj.grades)/no.rests.graded)
    if(nrow(adj.gr.props)==0) stop("grade proportions dataframe for unadjusted grading has 0 rows")
    names(adj.gr.props)<- c("grade", "proportion")
    # Calculate the difference between the proportions in adjusted grading and the desired proportions:
    diff.aj.desired<- adj.gr.props$proportion[index.to.update] - desired.props[index.to.update]
    # Update gamma_lower or gamma_upper
    if(diff.aj.desired < 0){
      gamma_lower <- gamma.perc[index.to.update]
    } else if(diff.aj.desired > 0){
      gamma_upper <- gamma.perc[index.to.update]
    }
    # call the function to continue updating process if you have not reached required tolerance
    if(abs(diff.aj.desired) > restaurant.tol/no.rests.graded){
      # Update gamma[index.to.update] depending on whether gamma_upper and gamma_lower already exist
      if(is.na(gamma_upper)==FALSE & is.na(gamma_lower)==FALSE){
        gamma.perc[index.to.update] <- 0.5*(gamma_upper + gamma_lower)
      } else{
        ## If gamma_upper and gamma_lower do not exist:
        new.gamma.val <- gamma.perc[index.to.update] - diff.aj.desired
        ## Ensure that new gamma value is greater than (or equal to) 0 and less than (or equal to) 1
        if(new.gamma.val >= 0 & new.gamma.val <= 1){
          gamma.perc[index.to.update] <- new.gamma.val
        }else{ ## if not, then round to 0/1
          gamma.perc[index.to.update] <- round(new.gamma.val)
        }
      }
      print.stmt<- paste("updating gamma_", index.to.update, sep="")
      print.stmt<- paste(print.stmt, ". Diff adjusted.proportion and desired.proportion: ", sep="")
      print.stmt<- paste(print.stmt, diff.aj.desired)
      print.stmt<- paste(print.stmt, ". New value:", sep="")
      print.stmt<- paste(print.stmt, gamma.perc[index.to.update], sep="")
      print.stmt<- paste(print.stmt, ". current iteration = ", sep="")
      print.stmt<- paste(print.stmt, iter, sep=" ")
      #print(print.stmt)
      iter<- iter + 1
      updateGamma(scores, z, desired.props, gamma.perc, index.to.update = index.to.update, restaurant.tol = restaurant.tol, iter = iter, max.iterations = max.iterations, gamma_upper = gamma_upper, gamma_lower = gamma_lower, resolve.ties = resolve.ties)
    } else{
      #print(paste("final gamma: ", gamma.perc[index.to.update], sep=""))
      return(gamma.perc[index.to.update])
    }
  }



























