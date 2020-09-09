
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
#' returns a long version of a table
#'
#' @export
its_wide_to_long_time <- function(df, names_to = "group", values_to = "value") {
  tidyr::pivot_longer(df, tidyselect::everything(), names_to = {{names_to}}, values_to = {{values_to}} )
}


#'
#'returns a categoric scatter plot
#'
#' @export
its_categoric_scatter_time <- function(df, names_to = "group", values_to = "value", colour = "dodgerblue", join_tops = FALSE) {
  its_wide_to_long_time(df, names_to = names_to, values_to = values_to) %>%
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


#' returns a plot summarising hypothesis tests as a figure
#'
#' @export
its_summary_plot_time <- function() {
  df <- data.frame(

    group = rep(c("a","b"), 12),
    measurement = runif(24)
  )
  mean_df <-  df %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(mean_y = mean(measurement), sd_y = sd(measurement) ) %>%
    dplyr::ungroup()

  df %>%
    ggplot2::ggplot() +
    ggplot2::aes(group, measurement) +
    ggplot2::geom_jitter(width=0.05) +
    ggplot2::geom_col(ggplot2::aes(x = as.numeric(group), y = mean_y), data = mean_df, fill = "steelblue", alpha = 0.3, colour = "black") +
    ggplot2::geom_errorbar( ggplot2::aes(x = group, ymin = mean_y - sd_y, ymax = mean_y + sd_y), data = mean_df, inherit.aes = FALSE, width = 0.1) +
    ggplot2::geom_smooth( ggplot2::aes(x = as.numeric(group), y = measurement), method = lm, colour = "red", linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::annotate(geom = "text", y = (mean_df$mean_y[[2]] + 0.06), x = "b", label = "How likely is this line to be flat, given the error?",)
}


#' returns a multi categry line plot
#'
#' @export
its_multi_category_with_lines_time <- function() {

  y_means <- PlantGrowth %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(gmean = mean(weight))

  PlantGrowth %>% ggplot2::ggplot() +
    ggplot2::aes(group, weight) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(x = 1, y = y_means$gmean[[1]], xend = 2, yend = y_means$gmean[[2]], colour = "dodgerblue",linetype = "dashed") +
    ggplot2::geom_segment(x = 1, y = y_means$gmean[[1]], xend = 3, yend = y_means$gmean[[3]],linetype = "dashed" ) +
    ggplot2::geom_segment(x = 2, y = y_means$gmean[[2]], xend = 3, yend = y_means$gmean[[3]], colour = "darkorange", linetype = "dashed") +
    ggplot2::theme_minimal()
}

#' example interactions
#'
#' @export
its_interaction_example_time <- function(){
 p1 <-  tibble::tibble(
    condition = c(rep("a", 20), rep("b", 20), rep("a & b", 20) ),
    x = runif(60, 0,6),
    y = x * c(rep(1.3, 20), rep(2, 20), rep( 4, 20)) + rnorm(60)
  ) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x,y, colour = condition) +
    ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggthemes::theme_tufte()
  p2 <- tibble::tibble(
    condition = c(rep("control", 20), rep("a", 20), rep("b", 20), rep("a & b", 20) ),
    measurement =  c(rep(1,20), rep(1.3, 20), rep(2, 20), rep( 4, 20)) + rnorm(80)
    ) %>%
    ggplot2::ggplot() +
      ggplot2::aes( condition, measurement, group = 1)+
      ggplot2::stat_summary(ggplot2::aes(y = measurement), fun = "mean", geom = "bar", width = 0.5, fill = "steelblue" ) +
      ggthemes::theme_tufte()
  cowplot::plot_grid(p1, p2, nrow = 1)
}

#' hot dog and ice cream box plots
#'
#' @export
its_hot_dog_and_ice_cream_plot_time <- function() {
  food <- its_hot_dog_and_ice_cream_time()
  plot_condiment <- food %>%
    #  group_by(Condiment) %>%
    #  summarise(mean_enj = mean(Enjoyment)) %>%
    dplyr::mutate(cond_num = dplyr::if_else( Condiment == "Chocolate Sauce", 1, 2) ) %>%
    ggplot2::ggplot() + ggplot2::aes(Condiment, Enjoyment) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.1) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = cond_num, y = Enjoyment),
      method = lm,
      colour = "red",
      linetype = "dashed"
    ) + ggplot2::theme_minimal()


  plot_food <- food %>%
    dplyr::mutate(food_num = dplyr::if_else( Food == "Hot Dog", 1, 2) ) %>%
    ggplot2::ggplot() + ggplot2::aes(Food, Enjoyment) +

    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.1) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = food_num, y = Enjoyment),
      method = lm,
      colour = "red",
      linetype = "dashed"
    )  + ggplot2::theme_minimal()

cowplot::plot_grid(plot_condiment, plot_food, nrow = 1)
}

#' plot interaction in food and condiment
#'
#' @export
its_hot_dog_and_ice_cream_two_ways_time <- function() {
  its_hot_dog_and_ice_cream_time() %>%
    dplyr::mutate(food_num = dplyr::if_else( Food == "Hot Dog", 1, 2) ) %>%
    ggplot2::ggplot() + ggplot2::aes(Food, Enjoyment) +
    ggplot2::geom_jitter( ggplot2::aes(colour = Condiment)) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = food_num, y = Enjoyment, colour = Condiment),
      method = lm,
      se = FALSE
    )  + ggplot2::theme_minimal()

}


#' plots of goodness of the mean
#' @param n number of points to generate
#' @param type hist or jitter type of plot to return
#' @export
its_is_the_mean_a_good_summary_time <- function(n, type = "hist") {

  if (n %% 2 == 1){
    n <- n + 1
  }
  b = 0.2
  df <- data.frame(
    distribution = c(
      rep("normal", n),
      rep("wide_normal",n),
      rep("uniform", n),
      rep("skew", n),
      rep("multimodal",n)
    ),
    values = c(
      rnorm(n),
      rnorm(n, 0, 4),
      runif(n, -3, 3),
      fGarch::rsnorm(n, mean=0, sd = 1, xi = 2.5),
      c(rnorm(n/2, -2, 1), rnorm(n/2, 2,1))

    )
  )

  df$distribution <- factor(df$distribution, levels = c("normal",
                                                        "wide_normal",
                                                        "uniform",
                                                        "skew",
                                                        "multimodal"
  ))
  summ_df <- dplyr::group_by(df, distribution) %>%
    dplyr::summarize(mean = mean(values) )


  if(type == "hist"){
  return(
    ggplot2::ggplot(df) +
    ggplot2::aes(values) +
    ggplot2::geom_histogram(binwidth = b) +
    ggplot2::geom_density(aes(y = b * ..count..) ) +
      ggplot2::geom_vline(data = summ_df, ggplot2::aes(xintercept = mean)) +
      ggplot2::facet_wrap(~ distribution, scales = "free_x") +
    ggthemes::theme_tufte()
  )
  }
  else {
    return(
      ggplot2::ggplot(df) +
        ggplot2::aes(distribution, values) +
        ggplot2::geom_boxplot() +
        ggplot2::geom_jitter() +
        ggplot2::facet_wrap(~ distribution, scales = "free_x") +
    ggthemes::theme_tufte()
    )
  }
}

