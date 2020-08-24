
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
#' Generate some x and y data in a dataframe
#'
#' @param n number of points
#' @param min minimum random value
#' @param max maximum random value
#' @param mult how much bigger on average y is than x
#' @param seed random seed
#' @export
#'
its_random_xy_time <- function(n, min = 5, max = 15, mult = 2, seed = "456" ) {
  set.seed(seed)
  tibble::tibble(
     x = runif(n, min, max),
     y = x * mult + rnorm(n)
  )
}
#' plot xy data
#'
#' @param df dataframe with columns x and y
#' @param line draw the computed line
#' @param residuals draw the residuals
#' @export
#' @return ggplot
#'
its_plot_xy_time <- function(df, line = FALSE, residuals = FALSE) {
  p <- ggplot2::ggplot(df) +
    ggplot2::aes(x,y) +
    ggplot2::geom_point() +
    ggthemes::theme_tufte()
  if (line){
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", colour = "dodgerblue")
  }
  if (residuals){
    model <- lm(y ~ x, data=df)
    res_data <- tibble::tibble(
      x = df$x,
      y = df$y,
      residual = predict(model)
    )
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = x, yend = residual), data = res_data)
  }
  p
}

#' generate a barplot
#' @param names_to what to call the variable containing the variable names
#' @param values_to what to call the variable containing the values
#' @param colour colour of the bars
#' @param join_tops add a line joining the tops of bars
#' @param points show individual points (geom_jitter)
#' @export
#' @return ggplot
#'
its_barplot_time <- function(df, names_to = "group", values_to = "value", colour = "dodgerblue", join_tops = FALSE, points = FALSE) {
  grouped <- tidyr::pivot_longer(df, tidyselect::everything(), names_to = {{names_to}}, values_to = {{values_to}} ) %>%
    dplyr::ungroup()
  #   dplyr::group_by( .data[[ {{names_to}} ]] ) %>% dplyr::summarize(mean = mean(.data[[{{values_to}}]]))
  #
  #
  #   p <- ggplot2::ggplot(grouped) +
  #   ggplot2::aes( x = .data[[ {{ names_to }} ]], y = mean ) +
  #   ggplot2::geom_bar(stat = "identity", width = 0.5, fill = colour) +
  #   ggthemes::theme_tufte()
  #
  #   if (join_tops){
  #
  #     p <- p + ggplot2::stat_summary(position = "identity", geom = "line", data = df)
  #   }
  #   p

  p <- ggplot2::ggplot(grouped) +
    ggplot2::aes( .data[[{{names_to}}]], .data[[{{values_to}}]], group = 1)+
    ggplot2::stat_summary(ggplot2::aes(y = .data[[{{values_to}}]]), fun = "mean", geom = "bar", width = 0.5, fill = colour ) +
    ggthemes::theme_tufte()

    if (points){
      p <- p + ggplot2::geom_jitter()
    }
    if (join_tops){
      p <- p + ggplot2::stat_summary(ggplot2::aes(colour = as.numeric(.data[[{{values_to}}]]) ), fun = "mean", geom = "line" )
    }
  p

}

#'
#'returns a categoric scatter plot
#'
#' @export
its_categoric_scatter_time <- function(df, names_to = "group", values_to = "value", colour = "dodgerblue", join_tops = FALSE) {
  df %>% tidyr::pivot_longer( tidyselect::everything(), names_to = {{names_to}}, values_to = {{values_to}} ) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[ {{names_to}}]], y = .data[[ {{values_to }} ]] ) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal()
}


#'
#' returns a straight line plot with a bend in it
#' @export
#'
its_bendy_line_time <- function() {
  data.frame(x = c(1,2,3), y = c(1,4,5)) %>%
  ggplot2::ggplot() +
    ggplot2::aes(x,y) +
    ggplot2::geom_line() +
    ggthemes::theme_tufte()

}

#'
#'returns a multidimensional plot
#' @export
#'
its_three_variable_plot_time <- function() {
  df <- tibble::tibble(
    x = runif(10, 5, 10),
    z = x * 3 + rnorm(10),
    y = x + z + rnorm(10)
  )
  scatter <- function(){
    s3d <- scatterplot3d::scatterplot3d(df, type="h", angle = 55,color = "dodgerblue")
    mod <- lm(y ~ x + z, data = df)
    s3d$plane3d(mod)
  }

  p <- df %>% tidyr::pivot_longer(-y, names_to = "var") %>%
    ggplot2::ggplot() +
    ggplot2::aes(value, y) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ var, scales = "free_x") +
    ggplot2::theme_minimal()

cowplot::plot_grid(scatter, p, ncol = 1, rel_heights = c(0.65, 0.35))

}






