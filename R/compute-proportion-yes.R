# function to compute proportion Yes on  any constituency

#' Computation underlying \code{question_split} and \code{issue_split}
#'
#' @rdname split
#' @param grptbl A grouped data frame with columns \code{response} and \code{weight}.
#'
#' @export
do_split <- function(grptbl) {
  sums <- grptbl %>%
  filter(!is.na(response), !is.na(weight)) %>%
  summarise(yes_wgt = sum(weight * (response == "Y")), # Y's get 1, rest 0. inner product with weight. Sum.
            no_wgt  = sum(weight * (response == "N")),
            dk_wgt  = sum(weight * (response == "DK")),
            yes_raw = sum(response == "Y"),
            no_raw  = sum(response == "N"),
            n_wgt   = sum(weight),
            n_eff   = (sum(weight)^2) / sum(weight^2),
            n_raw   = n(),
            .groups = "drop")
  sums |>
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
  groupby <- quos(q_label, year, !!!var)
  groupby2 <- quos(q_label, !!!var)

  # usually response NA means the question was not asked, so they should be
  # dropped rather than not counted in the numerator

  grp.tbl <- tbl %>%
    filter(!is.na(response)) %>%
    group_by(!!! groupby) %>%
    do_split() |>
    # then sum across years, weighting by sample size
    group_by(!!! groupby2) %>%
    summarize(
      across(matches("(yes|no)_raw"), sum),
      across(matches("(yes|no|dk)_wgt"), sum),
      pct_yes2_raw = weighted.mean(pct_yes2_raw, n_raw),
      pct_yes2 = weighted.mean(pct_yes2, n_raw), # weight by sample size (not necessarily weighted)
      across(matches("n_"), sum),
      .groups = "drop"
    )

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
