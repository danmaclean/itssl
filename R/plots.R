
linfunc <- function(x,a,b) {
  (x * a) + b
}

#' Draw a straight line graph
#'
#' @param xrange the extent of the x-axis
#' @param yrange the (optional) extent of the y-axis
#' @param a the slope of the line
#' @param b the intercept of the line
#' @export
#' @return a ggplot2 plot
#' @examples
#' its_axplusb_time()
#' its_axplusb_time(xrange = c(-10, 10), a = 4, b = 3)
its_axplusb_time <- function(
  xrange = c(-4,4),
  yrange = NULL,
  a=1,
  b=0
  ) {
  p <- ggplot2::ggplot(data.frame(x=xrange)) +
    ggplot2::aes(x = xrange) +
    ggplot2::stat_function(
      fun = linfunc,
      args = list(a,b)
    ) +
    ggthemes::theme_tufte() +
    ggplot2::xlim(xrange) +
    ggplot2::xlab("x") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted")

  if (!is.null(yrange)){
    p <- p + ggplot2::ylim(yrange)
  }
  p
}
#' Add another line to an existing plot
#'
#' @param a the slope of the new line
#' @param b the intercept of the new line
#' @param colour colour of the new line
#' @export
#' @return a ggplot2 geom
#' @examples
#' its_axplusb_time() + its_add_line_time(a=4, colour="orange")
#'
its_add_line_time <- function(a=1, b=0, colour="dodgerblue") {
  ggplot2::geom_abline(slope=a, intercept=b, colour=colour)
}
