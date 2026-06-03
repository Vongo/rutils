#' Logical evaluation that treats NA as FALSE
#'
#' Binarily evaluates a logical expression with NA giving FALSE
#' @param x logical expression to evaluate (coerced to logical; names preserved)
#' @return a logical vector: TRUE where x is TRUE, FALSE where it is FALSE or NA.
#'   Zero-length input returns the scalar FALSE.
#' @keywords NA
#' @seealso na.true
#' @export
#' @examples
#' na.false(FALSE) == na.false(NA)
na.false <- function(x) {
	if (length(x) == 0L) return(FALSE)
	nm <- names(x)
	x <- as.logical(x)
	x[is.na(x)] <- FALSE
	names(x) <- nm
	x
}

#' Logical evaluation that treats NA as TRUE
#'
#' Binarily evaluates a logical expression with NA giving TRUE
#' @param x logical expression to evaluate (coerced to logical; names preserved)
#' @return a logical vector: FALSE where x is FALSE, TRUE where it is TRUE or NA.
#'   Zero-length input returns the scalar TRUE.
#' @keywords NA
#' @seealso na.false
#' @export
#' @examples
#' na.true(TRUE) == na.true(NA)
na.true <- function(x) {
	if (length(x) == 0L) return(TRUE)
	nm <- names(x)
	x <- as.logical(x)
	x[is.na(x)] <- TRUE
	names(x) <- nm
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
