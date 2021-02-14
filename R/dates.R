#' Month difference (vectorized)
#'
#' Month difference between two date vectors of same length.
#' @param id1 input date vector (formatted character, IDate, POSIXlt, POSIXct, Date, etc.)
#' @param id2 input date vector (formatted character, IDate, POSIXlt, POSIXct, Date, etc.)
#' @param absolute should the time difference be absolute, or oriented.
#' When `absolute=FALSE`, `d1` is expected to be before `d2`. Hence returned value will be negative if it's not the case.
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
	ret <- 12*(data.table::year(id2)-data.table::year(id1)) + (data.table::month(id2)-data.table::month(id1))
	`if`(absolute, abs(ret), ret)
}
