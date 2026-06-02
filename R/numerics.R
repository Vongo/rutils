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
	vapply(x, function(s) {
		if (is.na(s)) return(NA_real_)
		# format() with scientific=FALSE avoids the "1e+20" path that broke digit
		# counting; stripping the sign avoids treating "-42" as a 3-digit number.
		txt <- format(s, scientific = FALSE, trim = TRUE)
		int_digits <- sub("\\..*$", "", sub("^-", "", txt))
		precision <- if (nchar(int_digits) <= 1L) {
			dec <- if (grepl("\\.", txt)) sub("^[^.]*\\.", "", txt) else ""
			min(floor(nchar(dec) / 2L), 2L)
		} else {
			-round(2 * nchar(int_digits) / 3)
		}
		round(s, precision)
	}, numeric(1))
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
bucket <- function(v, ncut=10, round.clever=FALSE) {
	if (all(is.na(v))) return(rep(NA_integer_, length(v)))
	splits <- quantile(v, probs=seq(0, 1, by=1/ncut)[2:(ncut+1)], na.rm=TRUE)
	if (round.clever) {
		splits <- round_clever(splits)
	}
	# findInterval is O(n log k) and NA-safe (NA -> NA), vs the old O(n*k) scan.
	pmin(findInterval(v, splits) + 1L, length(splits))
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
bucket2 <- function(v, splits=quantile(v, probs=seq(0, 1, by=1/10)[2:(10+1)], na.rm=TRUE)) {
	if (all(is.na(v)) || all(is.na(splits))) return(rep(NA_integer_, length(v)))
	pmin(findInterval(v, splits) + 1L, length(splits) + 1L)
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
	if (length(v) == 0L) return(v)
	if (is.na(max)) max <- base::max(v, na.rm=TRUE)
	if (is.na(min)) min <- base::min(v, na.rm=TRUE)
	if (!is.na(na.value)) v[is.na(v)] <- na.value
	v <- pmin(pmax(v, min), max)   # ~120x faster than nested sapply(sapply(...))
	if (!is.na(na.post)) v[is.na(v)] <- na.post
	v
}
