#' Melt wide CCES data to long format
#'
#' Reshapes wide CCES data (one column per question) into long format
#' (one row per respondent-question), converting haven-labelled values
#' to character strings in the process.
#'
#' `melt_cces()` uses `data.table::melt()` and is superseded by
#' `melt_cces_tidy()`, which uses `tidyr::pivot_longer()` instead.
#'
#' @param tbl CCES dataset (wide format, typically from [haven::read_dta()])
#' @param q_expr Regex that selects the question columns to melt
#'
#' @returns A long data frame with columns `case_id`, `year`, `question`, and `response`.
#'
#' @import dplyr
#' @importFrom data.table melt as.data.table
#' @importFrom haven as_factor
#'
#' @examples
#' # Melt roll call vote questions from the 2014 CCES sample
#' melt_cces_tidy(cces14_sample, "CC14_325")
#'
#' # Melt gun control questions
#' melt_cces_tidy(cces14_sample, "CC14_320")
#'
#' @export
melt_cces <- function(tbl, q_expr) {
  lifecycle::signal_stage("superseded", "melt_cces()", "melt_cces_tidy()")

  rc <- select(tbl, case_id, year, matches(q_expr)) %>%
    mutate_at(vars(matches(q_expr)), as_factor) %>%
    mutate_at(vars(matches(q_expr)), as.character)

  rc.long <- melt(as.data.table(rc),
                  id.vars = c("case_id", "year"),
                  value.name = "response",
                  variable.name = "question",
                  variable.factor = FALSE,
                  value.factor = FALSE)

  return(rc.long)
}

#' @rdname melt_cces
#' @importFrom tidyr pivot_longer
#' @export
melt_cces_tidy <- function(tbl, q_expr) {
  tbl |>
    select(case_id, year, matches(q_expr)) |>
    mutate(across(matches(q_expr), \(x) as.character(as_factor(x)))) |>
    pivot_longer(
      cols = matches(q_expr),
      names_to = "question",
      values_to = "response"
    )
}
