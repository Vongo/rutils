#' Extended pie chart function
#'
#' This function is a shortcut to make an ordered pie chart with clear labels.
#' @param x a vector of numeric values
#' @param title the title of the chart
#' @return a plot object that represents a pie chart
#' @keywords pie chart plot
#' @export
#' @examples
#' pie2(saf(round(rnorm(150))))
pie2 <- function(x, title="") {
	w <- x[order(-x)]
	full_names <- sapply(seq(length(w)), function(i) paste0(names(w)[i], " : ", round(100*w[i]/sum(w), 1), "% (", w[i],")"))
	pie(w, labels=full_names, main=title)
}

pie3 <- function(x, title="") {
	w <- x[order(-x)]
	full_names <- sapply(seq(length(w)), function(i) paste0(names(w)[i], " : ", round(100*w[i]/sum(w), 1), "%"))
	pie(w[names(w)%ni% c("À qualifier") & !grepl("NA", names(w))][seq(20)], labels=full_names[names(w)%ni% c("À qualifier") & !grepl("NA", names(w))][seq(20)], main=title)
}

#' Extended lines chart function
#'
#' This function is a shortcut to plot several lines in one call.
#' @param y data.frame or numeric matrix, each line of which is a line to plot
#' @param x x values for each line to plot
#' @param ynames how to name each line in legend
#' @param main plot title
#' @param append TRUE to append those lines to an existing plot,
#' 		FALSE to create a new one
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @return a plot object that represents a line scatter plot
#' @keywords line lines chart plot
#' @export
#' @examples
#' data(iris)
#' lines2(iris[seq(4), seq(4)], ynames=as.character(seq(4)))
lines2 <- function(y, x=seq(ncol(y)), ynames="", main="", append=F, xlab="", ylab="") {
	stopifnot(is.matrix(y) || is.data.frame(y))
	ylim <- range(y, na.rm=T)
	if (!append) {
		plot(NA, xlim=c(1, length(x)), ylim=ylim, main=main, xlab=xlab, ylab=ylab)
	}
	colors <- rainbow(nrow(y))
	for (i in seq(nrow(y))) {
		lines(y=y[i,], x=x, col=colors[i])
	}
	legend("top", legend=ynames, col=colors, pch=16)
}

#' Simple stacked area chart using plotly
#'
#' This function is a shortcut to plot stacked area chart using plotly.
#' @param dt data.table to use data from
#' @param y y variable (typically, what you want to observe)
#' @param x x variable (typically, time)
#' @param group group variable (typically, what you want to compare)
#' @param yname label for y axis
#' @param xname label for x axis
#' @param gname label for group
#' @param title plot title
#' @return a plot object that represents a stacked area chart
#' @keywords area stack plotly
#' @export
plotly_stacked_area <- function(dt, x, y, group, xname=NULL, yname=NULL, gname=NULL, title=NULL) {
	library(plotly)
	groups <- dt[, .(m=max(get(y), na.rm=TRUE)), .(group=get(group))][order(-m), group]
	colors <- wesanderson::wes_palette("GrandBudapest2", length(groups), type="continuous")
	dat <- dt[order(get(x))] %>%
		.[, .(x=get(x), y=get(y), group=get(group))] %>%
		dcast(x~group, fun.aggregate=sum, value.var="y", fill=NA)
	p <- dat %>% plot_ly(x=~x, y=~get(groups[1]), name=groups[1], type="scatter", mode="none", stackgroup="one", fillcolor=colors[1])
	for (ig in seq(2, length(groups))) {
		p %<>% add_trace(data=dat, x=~x, y=dat[[groups[ig]]], name=groups[ig], fillcolor=colors[ig])
	}
	xname <- `if`(is.null(xname), x, xname)
	yname <- `if`(is.null(yname), y, yname)
	gname <- `if`(is.null(gname), group, gname)
	p %>% layout(title = `if`(is.null(title), paste0(yname, "~", xname, " grouped by ", gname), title),
		xaxis=list(title=xname, showgrid=FALSE),
		yaxis=list(title=yname, showgrid=FALSE)
	)
}
