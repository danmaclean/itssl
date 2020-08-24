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
