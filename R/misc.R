#' Summary Function
#'
#' This function is a shortcut for `summary(as.factor(x))`
#' @param vector vector you want to summarize
#' @keywords summary
#' @export
#' @examples
#' saf(sample(c(TRUE,FALSE), 100, rep=TRUE))
saf <- function(vector) {
	summary(as.factor(vector))
}

#' Relative Summary Function
#'
#' This function is a shortcut for `summary(as.factor(x))`
#' @param vector vector you want to summarize
#' @param pretty how pretty should the result be? `0` will be a raw 0-1 value, `2` will be a 0-100 character string with percentage symbol.
#' @keywords summary
#' @export
#' @examples
#' safr(sample(c(TRUE,FALSE), 100, rep=TRUE), p=1)
safr <- function(vector, pretty=c(0,1,2)[1]) {
	o <- saf(vector)
	if (pretty==2) {
		r <- round(100*o/sum(o),2)
		ret <- paste0(r, "%")
		names(ret) <- names(r)
		ret
	} else if (pretty==1) {
		round(100*o/sum(o),2)
	} else {
		o/sum(o)
	}
}

#' Reload Package
#'
#' Reloads package that was previously loaded.
#' This is useful for instance when some objects of one package were masked and you want to reload them in a different order, but without loosing your current session's variables.
#' This function requires to have devtools installed.
#' @param package_name name of the package you want to reload
#' @return logical that represents whether the package was properly reloaded
#' @keywords reload package
#' @export
#' @examples
#' reload_package("rutils")
reload_package <- function(package_name) {
	devtools::reload(pkgload::inst(package_name))
}

#' Test environment
#'
#' Checks if current environment is aimed at testing.
#' Specifically, it tests whether package \code{testthat} was loaded.
#' @return \code{TRUE} if current environment is for testing, \code{FALSE} otherwise.
#' @keywords check test
#' @export
#' @examples
#' is_test_environment()
#' \dontrun{
#' library(testthat)
#' is_test_environment()
#' }
is_test_environment <- function() {
	"testthat" %in% (.packages())
}

#' Get OS
#'
#' Identify the OS you're currently running on.
#' @return 3-lower-cased-characters string
#' @keywords OS
#' @author Hadley https://github.com/r-lib/rappdirs/blob/master/R/utils.r#L1
#' @export
#' @examples
#' get_os()
get_os <- function() {
	if (.Platform$OS.type == "windows") {
		"win"
	} else if (Sys.info()["sysname"] == "Darwin") {
		"mac"
	} else if (.Platform$OS.type == "unix") {
		"tux"
	} else {
		stop("Unknown OS")
	}
}

#' Confusion table
#'
#' Pretty and relative confusion table
#' @param r numeric or character vector, factor or ordered. Will be displayed as row in result.
#' @param c numeric or character vector, factor or ordered, the same length as r. Will be displayed as column in result.
#' @param scale.by 1 for row, 2 for column
#' @param pretty 0 for 0-1 numeric, 1 for 0-100 numeric, anything else for simple table.
#' @return a convenient confusion matrix
#' @keywords confusion table matrix
#' @export
#' @examples
#' tabler(iris$Species, iris$Sepal.Length>5.8, scale=1, p=1)
tabler <- function(r, c, scale.by=1, pretty=1) {
	ret <- apply(table(r,c), scale.by, function(x) x/sum(x))
	if (scale.by==1) ret <- t(ret)
	if (pretty==0) {
		round(ret, 4)
	} else if (pretty==1) {
		round(100*ret, 2)
	} else {
		ret
	}
}

#' List Objects
#'
#' Extends \code{ls()} to associate each object (in current environment) with its (human-readable) size.
#' @param up ["UsePryr"] \code{TRUE} to use \code{pryr::object_size}, \code{FALSE} to use \code{utils::object.size}
#' @keywords ls
#' @seealso ls object.size
#' @export
#' @examples
#' \dontrun{lsh()}
lsh <- function(up=FALSE) {
	matches <- c("b", "Kb", "Mb", "Gb", "Tb", "Pb")
	ob <- ls(env=rlang::caller_env(n=1))
	bitsize <- sapply(ob, function(o) `if`(up, pryr::object_size, utils::object.size)(get(o, env=rlang::caller_env(n=1))))
	res <- data.frame(
		name=ob,
		size=sapply(bitsize, function(size) {
			if (size>0) {
				coeff <- log10(size) %/% 3
				paste(round(size/(10^(coeff*3)),2), matches[coeff + 1])
			} else {
				"O b"
			}
		})
	)
	res <- res[order(-bitsize), ]
	rownames(res) <- NULL
	base::print.data.frame(res)
	invisible(res)
}
