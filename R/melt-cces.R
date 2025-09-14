#' melt wide cces (raw versions) to correct var names
#'
#' @param tbl CCES dataset
#' @param q_expr regex that selects questions
#' @export
#'
#' @import dplyr
#' @importFrom data.table melt as.data.table
#'
melt_cces <- function(tbl, q_expr){

  # drop unnecessary columns and manipulate
  rc <- select(tbl, case_id, year, matches(q_expr)) %>%
    mutate_at(vars(matches(q_expr)), as_factor) %>% # make labels from dta info
    mutate_at(vars(matches(q_expr)), as.character) # make character so we can bind without error

  # MELT
  rc.long <- melt(as.data.table(rc),
                  id.vars = c("case_id", "year"),
                  value.name = "response",
                  variable.name = "question",
                  variable.factor = FALSE,
                  value.factor = FALSE)

  return(rc.long)
}

