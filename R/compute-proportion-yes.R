# function to compute proportion Yes on  any constituency

#' Computation underlying ques_split and issue_split
#'
#' @rdname split
#'
#' @export
do_split <- function(grptbl) {
  grptbl %>%
  filter(!is.na(response), !is.na(weight)) %>%
  summarise(yes_wgt = sum(weight * (response == "Y")), # Y's get 1, rest 0. inner product with weight. Sum.
            no_wgt  = sum(weight * (response == "N")),
            wgt_dk  = sum(weight * (response == "DK")),
            yes_raw = sum(response == "Y"),
            no_raw  = sum(response == "N"),
            n_wgt   = sum(weight),
            n_eff   = (sum(weight)^2) / sum(weight^2),
            n_raw   = n()) %>%
    ungroup() %>%
    mutate(pct_yes2_raw = yes_raw / (yes_raw + no_raw),
           pct_yes2 = yes_wgt / (yes_wgt + no_wgt))
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
