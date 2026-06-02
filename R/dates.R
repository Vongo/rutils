#' Month difference (vectorized)
#'
#' Number of calendar-month boundaries crossed between two dates (the day of the
#' month is ignored, so e.g. Jan 31 -> Feb 1 counts as 1). Inputs are coerced
#' with \code{data.table::as.IDate}; only date formats it understands parse
#' correctly. The two arguments must have the same length, or one must be
#' length 1 (it is then recycled).
#' @param id1 input date vector (formatted character, IDate, POSIXlt, POSIXct, Date, etc.)
#' @param id2 input date vector (formatted character, IDate, POSIXlt, POSIXct, Date, etc.)
#' @param absolute should the time difference be absolute, or oriented.
#' When `absolute=FALSE`, `id1` is expected to be before `id2`. Hence the returned value will be negative if it's not the case.
#' @return numeric value representing the number of months between two dates. If `absolute` is set to `TRUE`,
#' this difference will always be positive.
#' @keywords month date difference
#' @export
#' @examples
#' mondf("2019-01-01", as.Date(Sys.time()))
#' mondf(as.Date(Sys.time()), "2019-01-01", absolute=FALSE)
#' mondf(c("2019-01-01", "2018-01-01"), as.Date(Sys.time()))
#' mondf(c("2019-01-01", "2018-01-01"), rep(as.Date(Sys.time()), 2))
mondf <- function(id1, id2, absolute=TRUE) {
	id1 <- data.table::as.IDate(id1)
	id2 <- data.table::as.IDate(id2)
	if (length(id1) != length(id2) && length(id1) != 1L && length(id2) != 1L) {
		stop("mondf(): id1 and id2 must have the same length (or one must be length 1).")
	}
	ret <- 12*(data.table::year(id2)-data.table::year(id1)) + (data.table::month(id2)-data.table::month(id1))
	`if`(absolute, abs(ret), ret)
}
