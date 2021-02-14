symbols <- c(letters, 0:9, toupper(letters), strsplit("_ ./!,;:?", "")[[1]])

#' Naive encryption function
#'
#' Naive bijective encryption function
#' @param x character to encrypt
#' @param key random seed
#' @return a character string, the same length as x, but non-human readable
#' @keywords encryption cipher
#' @seealso decry
#' @export
#' @examples
#' a <- cry("Love is in the air", 1234)
#' print(a)
#' print(decry(a, 1235))
#' print(decry(a, 1234))
cry <- function(x, key=123) {
	set.seed(key)
	chartr(paste(symbols, collapse=""), paste(symbols[order(sample(seq(length(symbols)), length(symbols)))], collapse=""), x)
}

#' Naive decryption function
#'
#' Naive bijective decryption function
#' @param x character to decrypt
#' @param key random seed that was used to encrypt
#' @return a character string, the same length as x, that should be human-readable if you did it right.
#' @keywords encryption cipher
#' @seealso cry
#' @export
#' @examples
#' a <- cry("Love is in the air", 1234)
#' print(a)
#' print(decry(a, 1235))
#' print(decry(a, 1234))
decry <- function(x, key=123) {
	set.seed(key)
	chartr(paste(symbols[order(sample(seq(length(symbols)), length(symbols)))], collapse=""), paste(symbols, collapse=""), x)
}

#' Character trim function
#'
#' This functions removes trailing space characters at the begining and at the end of the input character vector.
#' @param s input character vector string
#' @return the same character vector without spaces at the begining or at the end
#' @keywords trim
#' @export
#' @examples
#' trim(" lorem ipsum	")
trim <- function(s) {
	sub("^[  ]+", "", sub("[  ]+$", "", s))
}

#' Title Case (on one word)
#'
#' Puts the first letter of the input character string in upper case, and the rest in lower case.
#' @param s input character vector string
#' @return the same character vector with only first letter in upper case
#' @keywords case title name
#' @export
#' @examples
#' titlecase_one("adrian")
titlecase_one <- function(s) {
	paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
}

#' Slug
#'
#' Reduces the input character string to a normal form.
#' This can be useful to match user-input names if you don't feel like doing any stemming/lemming.
#' @param x input character string
#' @param sep separator
#' @return the same character vector in its normal form
#' @keywords slug normal form
#' @export
#' @examples
#' slug("La magie d'Aladin")
slug <- function(x, sep="-") {
	sub(paste0("[", sep, "]+$"), "", sub(paste0("^[", sep, "]+"), "", gsub(paste0("[", sep, "]+"), sep, gsub("[^a-z^A-Z^0-9]+", "-", trim(tolower(iconv(x, to="ASCII//TRANSLIT")))))))
}

#' Fetch safe
#'
#' Simple wrapper around `curl::curl_fetch_memory` with a few safeguards
#' @param url url to fetch
#' @param max_attempts numeric that represents the maximum number of attempts
#' @param handle `curl::handle` to add to the connection
#' @param logger your custom logger if you want to keep track of potential warnings or errors
#' @return a list that represents the result of the fetch (with headers and content still binarized),
#' 		or NULL if `url` couldn't be fetched in the specified number of attempts.
#' @keywords curl_fetch_memory curl fetch
#' @seealso curl::curl_fetch_memory
#' @export
#' @examples
#' fetch_safe("http://www.qwant.com")
#' fetch_safe("http://www.qwant.comme")
fetch_safe <- function(url, max_attempts=3, handle=NULL, logger=NULL) {
	rurl <- encode_spaces(url)
	done <- F
	retry_count <- 0
	result <- NULL
	while (!done && retry_count < max_attempts) {
		tryCatch(
			{
				result <- if(is.null(handle)) {
					curl::curl_fetch_memory(rurl)
				} else {
 					curl::curl_fetch_memory(rurl, handle=handle)
				}
				done <- T
			},
			error=function(e) {
				if (!is.null(logger)) logging::logwarn("Error while fetching url [%s] (attempt %i): \n\t%s", rurl, retry_count+1, e, logger=logger)
			},
			finally={retry_count <- retry_count + 1}
		)
	}
	if (is.null(result)) {
		if (!is.null(logger)) logging::logerror("Error while fetching url [%s] (total attempts : %i).", rurl, retry_count, logger=logger)
	}
	result
}
