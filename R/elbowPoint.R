#' @title Determine the elbow point on a curve based on the maximum curvature along the curve
#' @description Given a list of x, y coordinates on a curve, function determines the elbow point of the curve.
#' @param x vector of x coordinates of points on the curve
#' @param y vector of y coordinates of points on the curve
#' @examples
#' # Generate some curve
#' x <- runif(100, min=-2, max=3)
#' y <- -exp(-x) * (1+rnorm(100)/3)
#' plot(x, y)
#' # Plot elbow points
#' abline(v=elbowPoint(x,y)$y, col="blue", pch=20, cex=3)
#' @return a list containing the (x, y) coordinates of the elbow point.
#' @details function is adapted from 'github.com/agentlans/KneeArrower'
#' @importFrom stats approx cor median predict smooth.spline
#' @importFrom stats approxfun optimize
#' @export
elbowPoint <- function(x, y) {
  # Test for non-numeric or infinite values
  is.invalid <- function(x) {
    any((!is.numeric(x)) | is.infinite(x))
  }
  if (is.invalid(x) || is.invalid(y)) {
    stop("x and y must be numeric and finite. Missing values not allowed.")
  }
  if (length(x) != length(y)) {
    stop("x and y must be of equal length.")
  }

  # Get value of curve at equally-spaced points
  new.x <- seq(from=min(x), to=max(x), length.out=length(x))
  # Use a spline which automatically smooths out noise
  sp <- smooth.spline(x, y)
  new.y <- predict(sp, new.x)$y

  # Finds largest odd number below given number
  largest.odd.num.lte <- function(x) {
    x.int <- floor(x)
    if (x.int %% 2 == 0) {
      x.int - 1
    } else {
      x.int
    }
  }

  # Use Savitzky-Golay filter to get derivatives
  smoothen <- function(y, p=p, filt.length=NULL, ...) {
    # Time scaling factor so that the derivatives are on same scale as original data
    ts <- (max(new.x) - min(new.x)) / length(new.x)
    p <- 3 # Degree of polynomial to estimate curve
    # Set filter length to be fraction of length of data
    # (must be an odd number)
    if (is.null(filt.length)) {
      filt.length <- min(largest.odd.num.lte(length(new.x)), 7)
    }
    if (filt.length <= p) {
      stop("Need more points to find cutoff.")
    }
    signal::sgolayfilt(y, p=p, n=filt.length, ts=ts, ...)
  }

  # Calculate first and second derivatives
  first.deriv <- smoothen(new.y, m=1)
  second.deriv <- smoothen(new.y, m=2)

  # Check the signs of the 2 derivatives to see whether to flip the curve
  # (Pick sign of the most extreme observation)
  pick.sign <- function(x) {
    most.extreme <- which(abs(x) == max(abs(x), na.rm=TRUE))[1]
    sign(x[most.extreme])
  }
  first.deriv.sign <- pick.sign(first.deriv)
  second.deriv.sign <- pick.sign(second.deriv)

  # The signs for which to flip the x and y axes
  x.sign <- 1
  y.sign <- 1
  if ((first.deriv.sign == -1) && (second.deriv.sign == -1)) {
    x.sign <- -1
  } else if ((first.deriv.sign == -1) && (second.deriv.sign == 1)) {
    y.sign <- -1
  } else if ((first.deriv.sign == 1) && (second.deriv.sign == 1)) {
    x.sign <- -1
    y.sign <- -1
  }
  # If curve needs flipping, then run same routine on flipped curve then
  # flip the results back
  if ((x.sign == -1) || (y.sign == -1)) {
    results <- elbowPoint(x.sign * x, y.sign * y)
    return(list(x = x.sign * results$x, y = y.sign * results$y))
  }

  # Find cutoff point for x
  cutoff.x <- NA
    # Find x where curvature is maximum
    curvature <- abs(second.deriv) / (1 + first.deriv^2)^(3/2)

    if (max(curvature) < min(curvature) | max(curvature) < max(curvature)) {
      cutoff.x = NA
    } else {
      # Interpolation function
      f <- approxfun(new.x, curvature, rule=1)
      # Minimize |f(new.x) - max(curvature)| over range of new.x
      cutoff.x = optimize(function(new.x) abs(f(new.x) - max(curvature)), range(new.x))$minimum
    }

   if (is.na(cutoff.x)) {
    warning("Cutoff point is beyond range. Returning NA.")
    list(x=NA, y=NA)
    } else {
    # Return cutoff point on curve
    approx(new.x, new.y, cutoff.x)
  }
}


