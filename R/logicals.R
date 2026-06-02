#' Logical evaluation that treats NA as FALSE
#'
#' Binarily evaluates a logical expression with NA giving FALSE
#' @param x logical expression to evaluate
#' @return TRUE if x is TRUE, FALSE if it is FALSE or NA
#' @keywords NA
#' @seealso na.true
#' @export
#' @examples
#' na.false(FALSE) == na.false(NA)
na.false <- function(x) {
	if (length(x) == 0L) return(FALSE)
	x <- as.logical(x)
	x[is.na(x)] <- FALSE
	x
}

#' Logical evaluation that treats NA as TRUE
#'
#' Binarily evaluates a logical expression with NA giving TRUE
#' @param x logical expression to evaluate
#' @return FALSE if x is FALSE, TRUE if it is TRUE or NA
#' @keywords NA
#' @seealso na.false
#' @export
#' @examples
#' na.true(TRUE) == na.true(NA)
na.true <- function(x) {
	if (length(x) == 0L) return(TRUE)
	x <- as.logical(x)
	x[is.na(x)] <- TRUE
	x
}

#' NOT IN function
#'
#' This function is a shortcut for `!(a %in% b)`
#' @param a contained vector
#' @param b containing vector
#' @return logical vector for each value in a, is there a match in b
#' @keywords in
#' @export
#' @examples
#' "a" %ni% letters[1:10]
`%ni%` <- function(a, b) {
	!(a%in%b)
}
