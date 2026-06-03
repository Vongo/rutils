#' Wide screen
#'
#' Fit R session output width to terminal window width.
#' @param ret return new terminal width
#' @keywords wide screen ws
#' @export
#' @return terminal width
#' @examples
#' \dontrun{ws()}
ws <- function(ret=TRUE) {
	# Fallback width for non-interactive / non-TTY contexts (cron, CI, pipes)
	# where `stty size` fails. getOption("width") is always a positive integer.
	fallback <- getOption("width", 80L)
	terminal_width <- if(get_os()=="tux") {
		tryCatch({
			sz <- suppressWarnings(system('stty size', intern=TRUE, ignore.stderr=TRUE))
			w <- as.numeric(strsplit(sz, ' ')[[1]])[2]
			if (length(w)!=1L || is.na(w) || w<=0) fallback else w
		}, error=function(e) fallback)
	} else {
		fallback
	}
	if (is.numeric(terminal_width) && terminal_width>0) {
		options(width=as.integer(terminal_width))
	}
	if (ret) terminal_width
}

#' Create Progress Bar
#'
#' Create an amazingly stylish progress bar.
#' @param nb_iter how many steps will there be
#' @param bar_style `simple` to have a simple progress bar, or `pc` if you want percentages
#' @param time_style `cd` to have a simple countdown, or `end` if you want an estimate of end date/time
#' @param width terminal width to draw into; detected from the terminal (falling back to 100) when NULL
#' @keywords progress bar
#' @export
#' @seealso update_pb
#' @examples
#' NB_ITER = 20
#' pb <- create_pb(NB_ITER)
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
#'
#' NB_ITER = 1000
#' pb <- create_pb(NB_ITER, bar_style="simple", time_style="cd")
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
#'
#' NB_ITER = 10000
#' # NOT RUN
#' pb <- create_pb(NB_ITER, bar_style="pc", time_style="end")
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
create_pb <- function(nb_iter,
                    bar_style=c("simple","pc"),
                    time_style=c("cd","end"),
                    width=NULL) {
    # No terminal attached and no explicit width (cron, CI, pipe) -> no progress
    # bar. The NULL is passed straight to update_pb(), which no-ops on a NULL bar.
    if (is.null(width) && !interactive() && !isatty(stdout())) return(invisible(NULL))
    ret <- list()
    ret$dep_time <- Sys.time()
    ret$tot_iter <- nb_iter
    ret$bar_style <- match.arg(bar_style)   # deterministic default; no sample() / RNG side effect
    ret$time_style <- match.arg(time_style)
    # Resolve the terminal width ONCE here and cache it, so update_pb() doesn't
    # spawn an `stty size` subprocess on every iteration. An explicitly supplied
    # width is validated loudly; only auto-detection failure falls back quietly.
    if (!is.null(width)) {
        if (!is.numeric(width) || length(width) != 1L || is.na(width) || width < 1) {
            stop("create_pb(): 'width' must be a single positive number.")
        }
        ret$width <- as.integer(width)
    } else {
        detected <- tryCatch(ws(ret=TRUE), error=function(e) NA_integer_)
        if (is.null(detected) || !is.numeric(detected) || is.na(detected)) {
            message("Can't detect terminal width, defaulting to 100.")
            detected <- 100L
        }
        ret$width <- as.integer(detected)
    }
    ret
}

#' Update Progress Bar
#'
#' Create an amazingly stylish progress bar.
#' @param pb progress bar previously created with `create_pb` (or NULL in a non-TTY context, in which case this is a no-op)
#' @param index progress level (bounded between 0 and pb`$tot_iter`, that was set by parameter `nb_iter` in `create_pb`)
#' @keywords progress bar
#' @export
#' @seealso create_pb
#' @examples
#' NB_ITER = 20
#' pb <- create_pb(NB_ITER)
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
#'
#' NB_ITER = 1000
#' pb <- create_pb(NB_ITER, bar_style="simple", time_style="cd")
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
#'
#' NB_ITER = 10000
#' # NOT RUN
#' pb <- create_pb(NB_ITER, bar_style="pc", time_style="end")
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
update_pb <- function(pb, index) {
    # No bar (non-TTY): create_pb() returned NULL, so there is nothing to draw.
    if (is.null(pb)) return(invisible(NULL))
    # Prefer the width cached by create_pb() (avoids spawning `stty` every tick);
    # fall back to ws() for hand-built bars. Coerce any non-numeric / invalid
    # return (e.g. a stale ws() that yielded a warning string) to 100.
    terminal_width <- if (!is.null(pb$width)) pb$width else tryCatch(ws(), error=function(e) 100L)
    if (!is.numeric(terminal_width) || length(terminal_width) != 1L ||
        is.na(terminal_width) || terminal_width <= 0) {
        terminal_width <- 100
    }

    # Clamp progress to [0, 1] so index > tot_iter never yields a negative bar
    # padding (strrep would error) and index == 0 / tot_iter == 0 never divides by zero.
    progress <- if (isTRUE(pb$tot_iter > 0)) min(max(index / pb$tot_iter, 0), 1) else 1
    elapsed <- Sys.time() - pb$dep_time

    # Prepare time display (undefined until some progress has been made)
    if (progress > 0) {
        exp_end <- pb$dep_time + elapsed / progress
        rmg_time <- exp_end - Sys.time()
        time <- if (pb$time_style == "cd") round(rmg_time, 2) else exp_end
    } else {
        time <- "?"
    }
    time_width <- nchar(as.character(time))

    # Prepare bar display; keep geometry non-negative on narrow/undetected widths.
    bar_width <- max(ifelse(pb$bar_style=="simple", terminal_width-time_width-6,
		terminal_width-time_width-10), 0)
    bar_nb <- max(min(floor(progress*bar_width), bar_width), 0)

    arrow <- if (bar_nb > 0 && bar_nb < bar_width) ">" else ""
    bar <- if (pb$bar_style == "simple")
        paste0("|", strrep("=", bar_nb), arrow, strrep(" ", bar_width-bar_nb), "| ")
    else
        paste0("|", strrep("=", bar_nb), arrow, strrep(" ", bar_width-bar_nb), "| ",
            floor(100*progress), "% | ")

    # Display
    cat(paste0("\r", strrep(" ", terminal_width)))
    cat(paste0("\r", bar, time))
	if (progress>=1) cat("\n")
}
