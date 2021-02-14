#' Clever rounding function
#'
#' This function tries to guess what is the best way to round a value to the closest meaningful value.
#' @param x numeric vector
#' @return a numeric vector rounded accordingly
#' @keywords round
#' @export
#' @examples
#' round_clever(c(123456789, 12345.6789, 1.2346789))
round_clever <- function(x) {
	sapply(x, function(s) {
	    l <- strsplit(as.character(s), "[.]")[[1]][1]
	    precision <- if (nchar(l) == 1 || nchar(l) == 2 && grepl("^-", l)) {
	        r <- strsplit(as.character(s), "[.]")[[1]][2]
	        min(floor(nchar(r)/2), 2)
	    } else {
	        -round(2*nchar(l)/3)
	    }
	    round(s, precision)
	})
}

#' Percentile-based bucketing function
#'
#' Associates each value of a numeric input vector to a percentile-based bucket.
#' @param v input numeric vector
#' @param ncut number of buckets to create
#' @param round.clever should values of v be rounded
#' @return a numeric vector where each value represents a class/bucket
#' @keywords bucket ditribution
#' @export
#' @examples
#' bucket(rnorm(100, 100, 25), 10, TRUE)
bucket <- function(v, ncut=10, round.clever=F) {
	splits <- quantile(v, probs=seq(0, 1, by=1/ncut)[2:(ncut+1)])
    if (round.clever) {
        splits <- round_clever(splits)
    }
	sapply(v, function(e) {
        min(seq(length(splits))[e<splits], length(splits))
	})
}

#' Threshold-based bucketing function
#'
#' Associates each value of a numeric input vector to a threshold-based bucket.
#' @param v input numeric vector
#' @param splits should values of v be rounded
#' @return a numeric vector where each value represents a class/bucket
#' @keywords bucket ditribution
#' @export
#' @examples
#' bucket2(rnorm(100, 100, 25), c(50, 75, 100, 125, 150))
bucket2 <- function(v, splits=quantile(v, probs=seq(0, 1, by=1/10)[2:(10+1)])) {
	sapply(v, function(e) {
        min(seq(length(splits))[e<splits], length(splits)+1)
	})
}

#' Numeric trim
#'
#' Trims a numeric input within a user-defined range
#' @param v input numeric vector
#' @param min minimum accepted value
#' @param max maximum accepted value
#' @param na.value value set to NA elements BEFORE minmax is applied
#' @param na.post [only if na.value is NA] value set to NA elements AFTER minmax is applied
#' @return a numeric vector where all values are between `min` and `max`
#' @keywords trim threshold min max
#' @export
#' @examples
#' minmax(rnorm(100, 100, 25), 75, 125)
#' summary((minmax(c(rnorm(1000000, 100, 25), rep(NA, 10)), 75, 125, na.value=-10)))
#' summary((minmax(c(rnorm(1000000, 100, 25), rep(NA, 10)), 75, 125, na.post=-10)))
minmax <- function(v, min=NA, max=NA, na.value=NA, na.post=NA) {
	w <- v
	if (is.na(max)) max <- max(w, na.rm=TRUE)
	if (is.na(min)) min <- min(w, na.rm=TRUE)
	if (!is.na(na.value)) w[is.na(w)] <- na.value
	w <- sapply(sapply(w, min, max), max, min)
	if (!is.na(na.post)) w[is.na(w)] <- na.post
	w
}
