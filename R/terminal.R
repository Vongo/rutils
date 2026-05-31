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
#' # NOT RUN
#' pb <- create_pb(NB_ITER, bar_style="pc", time_style="end")
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
create_pb <- function(nb_iter,
                    bar_style=sample(c("simple","pc"),1),
                    time_style=sample(c("cd","end"),1)) {
    # No terminal attached (cron, CI, pipe) -> no progress bar. The NULL is
    # passed straight to update_pb(), which no-ops on a NULL bar.
    if (!interactive() && !isatty(stdout())) return(invisible(NULL))
    ret <- list()
    ret$dep_time <- Sys.time()
    ret$tot_iter <- nb_iter
    ret$bar_style <- bar_style
    ret$time_style <- time_style
	tryCatch(ws(), error=function(e){
		cat(paste0("Warning: can't detect terminal width, defaulting to 100.\n"))
	})
    ret
}

#' Update Progress Bar
#'
#' Create an amazingly stylish progress bar.
#' @param pb progress bar previously created with `create_pb`
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
#' # NOT RUN
#' pb <- create_pb(NB_ITER, bar_style="pc", time_style="end")
#' \dontrun{for (i in 1:NB_ITER) {
#'     update_pb(pb,i)
#'     Sys.sleep(0.5)
#' }}
update_pb <- function(pb, index) {
    # No bar (non-TTY): create_pb() returned NULL, so there is nothing to draw.
    if (is.null(pb)) return(invisible(NULL))
    terminal_width <- tryCatch(ws(), error=function(e){
		100
	})
    # ws() must yield a positive number; guard against any non-numeric return
    # (e.g. a stale rutils whose ws() returned a warning string in non-TTY).
    if (!is.numeric(terminal_width) || length(terminal_width)!=1L ||
        is.na(terminal_width) || terminal_width<=0) {
        terminal_width <- 100
    }
    # Compute progress
    cur_iter <- index
    progress <- index/pb$tot_iter
    elapsed <- Sys.time() - pb$dep_time

    # Prepare time display
    total_time <- (elapsed/progress)
    exp_end <- pb$dep_time + total_time
    rmg_time <- exp_end - Sys.time()
    time <- if(pb$time_style=="cd")
        round(rmg_time,2)
    else
        exp_end
    time_width <- nchar(as.character(time))

    # Prepare bar display
    bar_width <- ifelse(pb$bar_style=="simple", terminal_width-time_width-6,
		terminal_width-time_width-10)
    # Keep the geometry sane on very narrow / undetected widths so the rep()
    # calls below never receive a negative count.
    bar_width <- max(1L, bar_width)
    bar_nb <- floor(progress*bar_width)
    bar_nb <- max(0L, min(bar_nb, bar_width))

    bar <- if (pb$bar_style == "simple")
        paste("|",paste(rep("=",bar_nb),collapse=""),
            ifelse(bar_nb>0 & bar_nb<bar_width,">",""),
            paste(rep(" ",bar_width-bar_nb),collapse=""),"| ",sep="")
    else
        paste("|",paste(rep("=",bar_nb),collapse=""),
            ifelse(bar_nb>0 & bar_nb<bar_width,">",""),
            paste(rep(" ",bar_width-bar_nb),collapse=""),"| ",
            floor(100*progress),"% | ",sep="")

    # Display
    cat(paste("\r",paste(rep(" ",terminal_width),collapse=""),sep=""))
    cat(paste("\r",bar,time,sep=""))
	if (progress>=1) cat("\n")
}
