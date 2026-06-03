symbols <- c(letters, 0:9, toupper(letters), strsplit("_ ./!,;:?", "")[[1]])

# Build the key-dependent substitution alphabet without leaking RNG state.
# set.seed() mutates the global .Random.seed; we snapshot and restore it so
# cry()/decry() are side-effect free. Computing the permutation up front (rather
# than inline as a chartr argument) also makes decry(cry(x, k), k) independent of
# argument-evaluation order, which the previous inline version got wrong.
.cipher_perm <- function(key) {
	has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
	if (has_seed) {
		old <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
		on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
	} else {
		on.exit(if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
			rm(list = ".Random.seed", envir = .GlobalEnv), add = TRUE)
	}
	set.seed(key)
	paste(symbols[order(sample.int(length(symbols)))], collapse = "")
}

#' Naive obfuscation function
#'
#' Naive bijective (monoalphabetic substitution) obfuscation. This is NOT
#' cryptographically secure: it is a trivially breakable substitution over a
#' fixed alphabet, and any character outside that alphabet passes through
#' unchanged. Use it for light obfuscation only, never to protect secrets.
#' @param x character to obfuscate
#' @param key random seed
#' @return a character string, the same length as x, but non-human readable
#' @keywords obfuscation cipher substitution
#' @seealso decry
#' @export
#' @examples
#' a <- cry("Love is in the air", 1234)
#' print(a)
#' print(decry(a, 1235))
#' print(decry(a, 1234))
cry <- function(x, key=123) {
	x <- as.character(x)
	chartr(paste(symbols, collapse=""), .cipher_perm(key), x)
}

#' Naive de-obfuscation function
#'
#' Reverses \code{\link{cry}} for a given key. See \code{cry} for the security
#' caveats: this is obfuscation, not encryption.
#' @param x character to de-obfuscate
#' @param key random seed that was used with \code{cry}
#' @return a character string, the same length as x, that should be human-readable if you used the right key.
#' @keywords obfuscation cipher substitution
#' @seealso cry
#' @export
#' @examples
#' a <- cry("Love is in the air", 1234)
#' print(a)
#' print(decry(a, 1235))
#' print(decry(a, 1234))
decry <- function(x, key=123) {
	x <- as.character(x)
	chartr(.cipher_perm(key), paste(symbols, collapse=""), x)
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
	# \p{Z} (perl) covers Unicode separators incl. NBSP (U+00A0), which
	# [[:space:]] / \s do not match; \s adds tab/newline/CR.
	sub("^[\\s\\p{Z}]+", "", sub("[\\s\\p{Z}]+$", "", s, perl=TRUE), perl=TRUE)
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
	out <- paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
	out[is.na(s)] <- NA_character_   # paste0 would otherwise turn NA into "NANA"
	out
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
	sub(paste0("[", sep, "]+$"), "", sub(paste0("^[", sep, "]+"), "", gsub(paste0("[", sep, "]+"), sep, gsub("[^a-zA-Z0-9]+", "-", trim(tolower(iconv(x, to="ASCII//TRANSLIT")))))))
}

#' Fetch safe
#'
#' Wrapper around \code{curl::curl_fetch_memory} that retries transport failures
#' with linear backoff, rejects HTTP error responses (status >= 400), and always
#' signals failure (via the supplied \code{logger} or a \code{warning()}) instead
#' of silently returning \code{NULL}.
#' @param url url to fetch
#' @param max_attempts maximum number of attempts (transport errors are retried)
#' @param handle `curl::handle` to add to the connection
#' @param logger your custom logger if you want to keep track of potential warnings or errors
#' @param backoff base seconds slept after a failed transport attempt; the wait grows
#'   linearly (backoff * attempt_number) and there is no sleep after the final attempt.
#'   HTTP errors (>= 400) are not retried. 0 disables sleeping.
#' @return a list that represents the result of the fetch (with headers and content still binarized),
#' 		or NULL if `url` couldn't be fetched successfully in the specified number of attempts.
#' @keywords curl_fetch_memory curl fetch
#' @seealso curl::curl_fetch_memory
#' @export
#' @examples
#' \dontrun{
#' fetch_safe("http://www.qwant.com")
#' fetch_safe("http://www.qwant.comme")
#' }
fetch_safe <- function(url, max_attempts=3, handle=NULL, logger=NULL, backoff=0.5) {
	max_attempts <- as.integer(max_attempts)
	if (is.na(max_attempts) || max_attempts < 1L) stop("fetch_safe(): max_attempts must be a positive integer.")
	if (!is.numeric(backoff) || length(backoff) != 1L || is.na(backoff) || backoff < 0) stop("fetch_safe(): backoff must be a non-negative number.")
	emit <- function(level, fmt, ...) {
		msg <- sprintf(fmt, ...)
		if (!is.null(logger)) {
			(if (level == "error") logging::logerror else logging::logwarn)(msg, logger=logger)
		} else {
			warning(msg, call.=FALSE)
		}
	}
	rurl <- utils::URLencode(url)
	result <- NULL
	retry_count <- 0L
	while (is.null(result) && retry_count < max_attempts) {
		retry_count <- retry_count + 1L
		fetched <- tryCatch(
			if (is.null(handle)) curl::curl_fetch_memory(rurl) else curl::curl_fetch_memory(rurl, handle=handle),
			error=function(e) {
				emit("warn", "Error while fetching [%s] (attempt %i): %s", rurl, retry_count, conditionMessage(e))
				NULL
			}
		)
		if (is.null(fetched)) {
			if (retry_count < max_attempts && backoff > 0) Sys.sleep(backoff * retry_count)
			next
		}
		if (fetched$status_code >= 400L) {       # HTTP errors are deterministic: don't retry, don't double-signal
			emit("warn", "HTTP %d while fetching [%s].", fetched$status_code, rurl)
			return(NULL)
		}
		result <- fetched
	}
	if (is.null(result)) emit("error", "Failed to fetch [%s] after %i attempt(s).", rurl, retry_count)
	result
}
