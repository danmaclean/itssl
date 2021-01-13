#' @export
its_bardata_time <- function() {
  tibble::tibble(group1 = runif(6, min = 4, max = 7),
                 group2 = runif(6, min = 5, max = 8 )
  )
}

#' @export
its_table_time <- function(df) {
  knitr::kable(df,align = "c")
}

#' removes a group from a dataframe
#'
#' @export
its_remove_a_group_time <- function(df, col = "group", level = "trt2") {
    res <- dplyr::filter(df, .data[[{{col}}]] != level) %>%
    droplevels()
    return(res)
}



#' compost data
#'
#' @export
#'
its_compost_time <- function() {
  supplement = factor(rep(c("Formula X1","Formula X2"), 16))
  compost = factor(rep(c("John Innes #1", "John Innes #2", "John Innes #2", "John Innes #1"), 8))
  size = rep(runif(32))
  tibble::tibble(
    supplement = supplement,
    compost = compost,
    size = size
  ) %>%
    dplyr::mutate(size = dplyr::if_else( (supplement == "Formula X1" & compost == "John Innes #2"), (size + 1), size) )
}

#' hr scores table
#'
#' @export
its_hr_score_scheme_time <- function() {
  tibble::tibble(
    severity = c("Dead", "Very Ill", "Ill", "No Effect"),
    score = c(4,3,2,1)
  )  %>% its_table_time()
}

#' hr score data
#'
#' @export
its_hr_scores_time <- function() {
  tibble::tibble(
    strain = rep(c("control", "mild", "deadly"),3),
    replicate = c(rep(1, 3), rep(2,3), rep(3,3)),
    score = c( 1, 3, 4, 2, 3, 4, 1, 3, 3)
  )
}

#' mendel data
#'
#' @export
its_mendel_data_time <- function() {
  set.seed(123)
  tibble::tibble(
    cross = sample(c("PP", "PW", "WP", "WW"), 600, replace = TRUE) ,
    result = dplyr::if_else(cross == "WW", "W", "P")
  )
}

#' mendel count data
#'
#' @export
its_mendel_count_data_time <- function() {
  its_mendel_data_time() %>%
    dplyr::count(result) %>%
    dplyr::rename(colour = result, count = n)
}

#' mendel frequency data
#'
#' @export
its_mendel_frequency_time <- function() {
  its_mendel_count_data_time() %>%
    tidyr::pivot_wider(names_from = c("colour"), values_from = c("count")) %>%
    dplyr::mutate(
      ratio_p = P / min(c(P, W)),
      ratio_w = W / min(c(P, W)),
      freq_p = P / (P + W),
      freq_w = W / (P + W)
    )
}

#' voter data
#'
#' @export
its_voting_data_time <- function() {
  data.frame(
    expand.grid(
      generation = c("boomer", "millenial"),
      alignment = c("fascist", "instagram", "marxist" )
    ),
    count = c(279, 165, 74, 47, 225, 191)
  )
}

#' job data
#'
#' @export
its_job_mood_time <- function() {
  data.frame(
    mood = c('curious', 'curious', 'tense', 'tense', 'whimsical', 'whimsical','tense', 'whimsical', 'whimsical'),
    role = c('milliner', 'carpenter', 'milliner', 'carpenter', 'milliner', 'carpenter', "cooper", "cooper", "cooper"),
    Freq = c(100, 70, 30, 32, 110, 120, 30, 32, 110)
  )
}

#' tutorial data
#'
#' @export
its_small_data_frame_time <- function() {
  data.frame(
    names = c("Guido", "Marty", "Alan"),
    age = c(24,45,11),
    score = runif(3) * 100
  )
}

#' food data
#'
#' @export
its_food_data_time <- function(n = 20) {
  set.seed("123")
  df1 <- data.frame(Food = rep("Tortilla Chips", n), Condiment = rep("Hummous", n), Enjoyment = rnorm(n, 90, 5) )
  df2 <- data.frame(Food = rep("Tortilla Chips", n), Condiment = rep("Jam", n), Enjoyment =  rnorm(n, 60, 4) )

  df3 <- data.frame(Food = rep("Porridge", n), Condiment = rep("Hummous",n), Enjoyment = rnorm(n, 60, 4))
  df4 <- data.frame(Food = rep("Porridge", n), Condiment = rep("Jam", n), Enjoyment = rnorm(n, 90, 5))
  df <- dplyr::bind_rows(list(df1,df2,df3,df4))
  df$Food <- factor(df$Food, levels = c("Porridge", "Tortilla Chips"))
  df$Condiment <- factor(df$Condiment, levels = c("Hummous", "Jam"))
  return(df)


}
