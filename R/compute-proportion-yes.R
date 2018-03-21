# function to compute proportion Yes on  any constituency

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

  groupby <- quos(year, cong, question, !!!var)

  grp.tbl <- tbl %>%
    filter(!is.na(response)) %>% # usually response NA means the question was not asked, so they should be dropped rather than not counted in the numerator
    group_by(!!! groupby) %>%
    summarise(wgtYes = sum(weight * (response == "Y")), # Y's get 1, rest 0. inner product with weight. Sum.
              wgtNo = sum(weight * (response == "N")),
              wgtDK = sum(weight * (response == "DK")),
              wgtN = sum(weight),
              rawN = n()) %>%
    ungroup() %>%
    collect(n = Inf) %>%
    mutate(pctY = wgtYes / wgtN,
           pctY2 = wgtYes / (wgtYes + wgtNo)) # 2-party vote idea

  # append qlabel
  left_join(grp.tbl,
            byq,
            by = c("year" = "cces_year", "question" = "q_code")) %>%
    select(-question)
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

  tblID <- left_join(tbl,
                     byq,
                     by = c("year" = "cces_year", "question" = "q_code")) %>%
    select(-question)


  grp.tbl <- tblID %>%
    filter(!is.na(response)) %>% # usually response NA means the question was not asked, so they should be dropped rather than not counted in the numerator
    group_by(!!! groupby) %>%
    summarise(wgtYes = sum(weight * (response == "Y")), # Y's get 1, rest 0. inner product with weight. Sum.
              wgtNo = sum(weight * (response == "N")),
              wgtDK = sum(weight * (response == "DK")),
              wgtN = sum(weight),
              rawN = n()) %>%
    ungroup() %>%
    collect(n = Inf) %>%
    mutate(pctY = wgtYes / wgtN,
           pctY2 = wgtYes / (wgtYes + wgtNo)) # 2-party vote idea

  grp.tbl
}
