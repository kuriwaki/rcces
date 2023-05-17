
#' cast two two columns per case_id
#' @param tbl A long dataset, where each row is a senator (senator-response)
#' @param cast_names The names of the variables in tbl that will be the \code{value.var} in dcast
#' @param names  New names
#'
#' @importFrom tidyr pivot_wider
#' @import dplyr
#'
#' @export
#'
#'
cast_senate <- function(tbl, cast_names) {

  tbl |>
    pivot_wider(
      id_cols = c(year, case_id, qID),
      names_from = rownum,
      values_from = cast_names,
      names_glue = "sen{rownum}_{str_remove(.value, 'sen_')}"
    )
}
