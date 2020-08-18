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
