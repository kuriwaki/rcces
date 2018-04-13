
#' cast two two columns per case_id
#' @param tbl A long dataset, where each row is a senator (senator-respobse)
#' @param cast_names The names of the variables in tbl that will be the \code{value.var} in dcast
#' @param names  New names
#'
#' @export
cast_senate <- function(tbl, cast_names, names) {

  dt <- as.data.table(tbl)

  wd <- dcast.data.table(dt,
                         formula = year + case_id + qID ~ rownum,
                         value.var = cast_names,
                         sep = "")

  wd2 <- tbl_df(wd) %>%
    rename(sen1_icpsr = sen_icpsr1) %>% # rename
    rename(sen2_icpsr = sen_icpsr2) %>%
    rename_(.dots = setNames("sen_dyad_agrmt1", names[1])) %>% # rename
    rename_(.dots = setNames("sen_dyad_agrmt2", names[2]))

  if ("vote" %in% colnames(tbl)) {
    wd2 <- wd2 %>%
      rename_(.dots = setNames("vote1", "sen1vote")) %>%
      rename_(.dots = setNames("vote2", "sen2vote"))
  }

  return(wd2)
}
