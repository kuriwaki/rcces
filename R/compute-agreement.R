

#' Compute dyadic agreement from dataframe
#'
#' @param tbl dataframe
#' @param chamber One of \code{"H", "S", "Policy"}
#' @param agrmt_name The name for thew new variable in character
#' @param svy_var The opinion variable to look at
#' @param policy_var The vote variable to look at
#'
#'@export
#'
dyad_agrmt <- function(tbl, chamber, agrmt_name = "agrmt", svy_var = "response", policy_var = "vote") {

  if (!chamber %in% c("H", "S", "Policy")) stop("Must be H or S")
  if (!all(c(svy_var, policy_var, "year", "case_id", "qID") %in% colnames(tbl))) stop("some columns missing")
  held_var <- glue("vote_held_{chamber}")
  icpsr_var <- glue("{recode(chamber, `H` = 'rep', `S` = 'sen', `Policy` = 'foo')}_icpsr") # for matching purposes

  # core
  tbl_agrmt <- tbl_df(tbl) %>%
    mutate(agrmt = NA) %>%
    mutate(agrmt = replace(agrmt, (.data[[svy_var]] == "Y" & .data[[policy_var]] == "Y") | (.data[[svy_var]] == "N" & .data[[policy_var]] == "N") | (.data[[svy_var]] == "N" & .data[[held_var]] == 0), 1),
           agrmt = replace(agrmt, (.data[[svy_var]] == "Y" & .data[[policy_var]] == "N") | (.data[[svy_var]] == "N" & .data[[policy_var]] == "Y") | (.data[[svy_var]] == "Y" & .data[[held_var]] == 0), -1))

  tbl_agrmt <- tbl_agrmt %>%
    mutate(
      # don't know / not sure on respondent's part
      agrmt = replace(agrmt, (.data[[svy_var]] %in% c("DK")), 0),
      # if respondent didn't answer, didn't get represented
      agrmt = replace(agrmt, (is.na(.data[[svy_var]])), 0),
      # if rep missed that vote
      agrmt = replace(agrmt, (.data[[svy_var]] %in% c("N", "Y") & .data[[policy_var]] %in% c("missing vote")), 0),
      # don't count if unelected
      agrmt = replace(agrmt, .data[[policy_var]] %in% c("not elected"), NA),
      # Senate prerogatives: Sotomayor, Kagan, Garland
      agrmt = replace(agrmt, qID %in% c("CC10_332E", "CC09_59G", "CC16_351A", "CC14_331_3", "CC17_340B", "CC17_340H") & chamber == "H", NA)
    )

  out <- select(tbl_agrmt, year, case_id, qID, matches(icpsr_var), matches("rownum"), !!agrmt_name := agrmt)

  return(out)
}


