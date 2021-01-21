#' \code{qcDive} Checks consistency of dive records
#'
#' @param d.dat Dive data to be quality controlled
#' @param flag.only logical specifying if erroneous records should be flagged or deleted
#' @param max.v.speed Threshold for maximum allowable rate of change in depth
#' between inflection points (in m/s)
#' @return Returns a data frame with either an added column with qc flag
#' (0 for OK, 1 for error), or with erroneous records deleted
#'   or an interactive dygraph (if \code{type} is set to 'dygraph')
#' @details the function checks for consistency in time sequences between
#' inflections points and unrealistic depth changes between inflection points.
#' @family SMRU SRDL functions
#' @seealso \code{\link{fields.SRDLdb}} for table field names,
#'   \code{\link{ref.SRDLdb}} for ref ID codes,
#'   \code{\link{dep.SRDLdb}} for retrieving deployments metadata,
#'   \code{\link{get.SRDLdb}} for querying and retrieving data from database table
#'   \code{\link{strip.SRDL}}makes a stripchart of SRDL dive data
#' @author Martin Biuw
#' @examples
#' dive.deleted <- qcDive(dive)
#' dive.flagged <- qcDive(flagged.only=T)
#' @export

qcDive <- function(d.dat=dive, flag.only=T, max.v.speed=5) {
  inflect <- match(paste('D', c(1:25), sep=''), names(d.dat))
  inflect <- inflect[which(!is.na(inflect))]
  inflect <- inflect[unlist(lapply(inflect, function(x) !all(is.na(d.dat[,x]))))]

  d.pts <- inflect
  t.pts <- match(gsub('D', 'T', names(d.dat)[inflect]), names(d.dat))

  deleted <- vector('numeric')
  for(i in 1:length(t.pts)) {
    deleted <- c(deleted, which(d.dat[,t.pts[1]]==100 | d.dat[,t.pts[1]]==0))
  }

  deleted <- c(deleted, which(apply(d.dat[,t.pts], 1, function(x) any(diff(x)<0))))

  require(progress)
  pb <- progress_bar$new(total=nrow(d.dat))

  for(i in 1:nrow(d.dat)) {
    pb$tick()
    x <- unlist(d.dat[i,c(t.pts, d.pts)])
    if(any(is.na(x))) {
      deleted <- c(deleted, i)
    } else {
      t.ind <- c(1:(length(x)/2))
      d.ind <- c(((length(x)/2)+1):length(x))
      df.t <- diff(unlist(x[t.ind]))
      df.d <- diff(unlist(x[d.ind]))
      if(any(df.t==0)) {
        deleted <- c(deleted, i)
      }
    }
  }

  t.diff <- pbapply::pblapply(c(1:(length(t.pts)-1)), function(y) {
    apply(as.matrix(d.dat[,t.pts]), 1, function(x) diff(x)[y])
  })

  d.diff <- pbapply::pblapply(c(1:(length(d.pts)-1)), function(y) {
    apply(as.matrix(d.dat[,d.pts]), 1, function(x) diff(x)[y])
  })

  t.diff <- as.matrix(as.data.frame(t.diff))
  t.diff <- (0.01*t.diff)*as.numeric(d.dat$DIVE.DUR)
  d.diff <- as.matrix(as.data.frame(d.diff))

  deleted <- c(deleted, which(apply(t.diff, 1, min)<0))
  speed <- abs(d.diff/t.diff)
  deleted <- c(deleted, which(apply(speed, 1, function(x) any(x>max.v.speed))))
  deleted <- c(deleted, which(apply(speed, 1, function(x) any(!is.finite(x)))))

  deleted <- sort(unique(deleted))
  if(flag.only) {
    d.dat$D.MASK <- rep(0, nrow(d.dat))
    d.dat$D.MASK[deleted] <- 1
    d.dat
  } else {
    d.dat[-deleted,]
  }
}

