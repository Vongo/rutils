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
#' @param split (default=\code{TRUE}) split the results by class
#' @keywords ls
#' @seealso ls object.size
#' @export
#' @examples
#' \dontrun{lsh()}
lsh <- function(up=FALSE, split=TRUE) {
	matches <- c("b", "Kb", "Mb", "Gb", "Tb", "Pb")
	env <- rlang::caller_env(n=1)
	obj <- ls(envir=env)
	sizer <- if (up) {
		if (!requireNamespace("pryr", quietly=TRUE)) stop("lsh(up=TRUE) requires the 'pryr' package.")
		pryr::object_size
	} else {
		utils::object.size
	}
	# Resolve each object once in the CALLER's environment, then derive both its
	# size and its class from that value. The previous version read the size from
	# the caller but the class via eval(parse(text=name)) in lsh's own frame,
	# which failed for any object not visible at top level (and re-ran user code).
	values <- lapply(obj, get, envir=env)
	bitsize <- vapply(values, function(v) as.numeric(sizer(v)), numeric(1))
	human <- vapply(bitsize, function(size) {
		if (size > 0) {
			coeff <- floor(log10(size) / 3)
			paste(round(size / (10^(coeff*3)), 2), matches[coeff + 1])
		} else {
			"0 b"
		}
	}, character(1))
	klass <- vapply(values, function(v) class(v)[1], character(1))
	res <- data.table::data.table(name=obj, size=human, bitsize=bitsize, class=klass)[order(-bitsize)]
	if (split) {
		for (cl in res[, .(mb=max(bitsize)), by=class][order(-mb), class]) {
			message(cl)
			base::print.data.frame(res[class==cl, .(name, size)])
			cat("\n")
		}
	} else {
		base::print.data.frame(res[, .(name, size, class)])
	}
	invisible(res)
}


#' Unique Remove-Nas Sort -> URNS
#'
#' Cleans a vector (removes NAs & duplicates, sorts values ascending). This is
#' the S3 generic; methods exist for numeric, character and logical, and a
#' default that handles any other sortable atomic type (factor, Date, ...).
#' @param x vector to clean
#' @keywords urns unique NA sort
#' @seealso \code{\link{urns.numeric}} \code{\link{urns.default}} sort unique
#' @export
#' @examples
#' urns(c(3, 1, 2, 1, NA))
urns <- function(x) {
	UseMethod("urns")
}

#' @describeIn urns default method; sorts unique non-NA values of any atomic type
#'   (factor, Date, POSIXct, ...). \code{sort()} drops NA values.
#' @export
#' @examples
#' urns(as.Date(c("2020-01-02", "2020-01-01", NA)))
urns.default <- function(x) {
	sort(unique(x))
}

#' @describeIn urns numeric method; coerces to numeric before cleaning
#' @export
#' @examples
#' urns(c("3", "1", "2", "x"))
urns.numeric <- function(x) {
	suppressWarnings(sort(unique(as.numeric(x))))
}

#' @describeIn urns character method; coerces to character before cleaning
#' @export
#' @examples
#' urns(sample(letters, 100, replace=TRUE))
urns.character <- function(x) {
	sort(unique(as.character(x)))
}

#' @describeIn urns logical method; coerces to logical before cleaning
#' @export
#' @examples
#' urns(c(TRUE, NA, FALSE, TRUE))
urns.logical <- function(x) {
	sort(unique(as.logical(x)))
}
