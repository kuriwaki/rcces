# function to compute proportion Yes on  any constituency

#' Computation underlying ques_split and issue_split
#'
#' @rdname split
#'
#' @export
do_split <- function(grptbl) {
  grptbl %>%
  filter(!is.na(response), !is.na(weight)) %>%
  summarise(wgt_yes = sum(weight * (response == "Y")), # Y's get 1, rest 0. inner product with weight. Sum.
            wgt_no  = sum(weight * (response == "N")),
            wgt_DK  = sum(weight * (response == "DK")),
            raw_yes = sum(response == "Y"),
            raw_no  = sum(response == "N"),
            wgt_N   = sum(weight),
            raw_N   = n()) %>%
    ungroup() %>%
    mutate(pct_yes = wgt_yes / wgt_N,
           pct_yes2 = wgt_yes / (wgt_yes + wgt_no))
}

#' Proportion Yes by topic
#'
#' Unlike \code{issue_split}, this question will aggregate across years if the same question is asked.
#'
#' @rdname split
#'
#' @param tbl The dataset with the columns \code{cong}, \code{question}
#' @param ... variables to group on in addition to year and question, tidyeval variables
#'
#' @export
#'
#'
issue_split <- function(tbl, ...) {

  var <- quos(...)
  groupby <- quos(q_label, !!!var)

  # usually response NA means the question was not asked, so they should be
  # dropped rather than not counted in the numerator

  grp.tbl <- tbl %>%
    filter(!is.na(response)) %>%
    group_by(!!! groupby) %>%
    do_split()

  grp.tbl
}

#' Proportion Yes by question
#'
#' Splits by qID, or a question asked in a given year. Does not aggregate across years
#'
#' @rdname split
#'
#' @param tbl The dataset with the columns \code{cong}, \code{question}
#' @param ... variables to group on in addition to year and question, tidyeval variables
#' @export
question_split <- function(tbl, ...) {

  var <- quos(...)
  groupby <- quos(q_label, year, cong, qID, !!!var)

  grp.tbl <- tbl %>%
    filter(!is.na(response)) %>%
    group_by(!!! groupby) %>%
    do_split()

  grp.tbl
}
