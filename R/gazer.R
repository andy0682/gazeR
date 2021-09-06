#' Application of the the DBSCAN clustering algorithm for eye tracking data.
#' @param dat A data frame or a matrix with three columns named: X, Y, time.
#' @param eps Size (radius) of the epsilon neighborhood.
#' @param minPts Number of minimum points required in the eps neighborhood for core points (including the point itself). The default value is 5 points.
#' @param recRate Time in ms between each eye-tracking recording. For instance 1 means 1 ms between two observations or a recording frequency of 1000 Hz.
#' @return Provides a numeric vector indicating wheather an eye-tracking recording belongs to an eye-movement or to a fixation.
#' zero 0 means that the recording belongs to an eye-movement. Any other positive number indicates a fixation ID.
#' In addition, this function also returns a vector of the fixations IDs and their durations.
#' @importFrom dbscan dbscan
#' @export
gazeClusters  <- function(dat, eps, minPts = 5, recRate = NULL) {

  if (missing(dat)) {
    stop("dat is required")
  }

  if(missing(eps)) {
    stop("eps is required")
  }


  if (is.character(unlist(dat))) {
    stop("dat content needs to be numeric")
  }

  if (!is.data.frame(dat) & !is.matrix(dat) ) {
    stop("dat should be a matrix or a data frame with three columns named: X, Y, time.")
  }

  if ( any(!c('time','X', 'Y') %in% colnames(dat)) ) {
    ### this should take care of column names and data with less than three columns
    stop("dat needs three columns named: X, Y, time.")
  }

  ### dbscan takes care of error messages for its own parameters (eps, minPts)
  clust <- dbscan(dat, eps=eps, minPts=minPts)
  clustersVec <- clust$cluster
  if (sum(clustersVec)  == 0 ) {
    warning('No fixations found. You may need to use other values for the eps or minPTS parameters')
  }

  fixationsSummary <-  aggregate(clustersVec, by= list(clustersVec), length)
  if (!is.null(recRate) ) {
    fixationsSummary$x <- fixationsSummary$x*recRate
  }

  structure(list(cluster = clustersVec, eps = eps, minPts = minPts, fixationID = fixationsSummary$Group.1, fixationLength = fixationsSummary$x ),
            class = c("dbscan_fast", "dbscan", "gazeR"))

}
