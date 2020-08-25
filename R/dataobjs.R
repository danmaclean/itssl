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


#' interaction data from Jim's stats site
#'
#' @export
#'
its_hot_dog_and_ice_cream_time <- function() {
 readr::read_csv("data/Interactions_Categorical.csv") %>%
    dplyr::transmute(Food = as.factor(Food), Condiment = as.factor(Condiment), Enjoyment )
}
