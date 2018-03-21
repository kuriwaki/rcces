# function to compute proportion Yes on  any constituency
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


# by issue, rather than by individiual question. similar to question_split
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
