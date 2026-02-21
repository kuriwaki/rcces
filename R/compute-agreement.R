

#' Compute dyadic agreement from dataframe.
#'
#' If vote was not held, assumes status quo. (i.e. doesn't drop)
#'
#' @param tbl dataframe
#' @param chamber One of \code{"H", "S", "Policy"}
#' @param agrmt_name The name for thew new variable in character
#' @param svy_var The opinion variable to look at
#' @param policy_var The vote variable to look at
#'
#'
#' @importFrom dplyr mutate case_when pull as_tibble
#' @importFrom magrittr `%>%`
#' @importFrom glue glue
#'
#'@export
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   year = 2016, case_id = 1:4, qID = "CC16_350a",
#'   response     = c("Y", "N", "Y", "DK"),
#'   vote         = c("Y", "Y", "N", "N"),
#'   vote_held_H  = c(1, 1, 1, 1),
#'   rep_icpsr    = 1001:1004
#' )
#' dyad_agrmt(df, chamber = "H")
dyad_agrmt <- function(tbl, chamber, agrmt_name = "agrmt", svy_var = "response", policy_var = "vote") {

  if (!chamber %in% c("H", "S", "Policy")) stop("Must be H or S")
  if (!all(c(svy_var, policy_var, "year", "case_id", "qID") %in% colnames(tbl))) stop("some columns missing")
  held_var <- glue("vote_held_{chamber}")
  icpsr_var <- glue("{recode(chamber, `H` = 'rep', `S` = 'sen', `Policy` = 'foo')}_icpsr") # for matching purposes

  # core
  tbl_agrmt <- as_tibble(tbl) %>%
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


#' simpler version of dyad agreement, intended for long form where office is set
#'
#' @param data dataset
#' @param var1 first variable of Y, N, DK, or D,R
#'
#'
#' @importFrom dplyr mutate case_when pull
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(pid_self =   c("D", "R", "I", "D"),
#'              pid_actl =   c("R", "R", "R", "D"),
#'              issue_self = c("Y", "N", "DK", "N"),
#'              issue_actl = c("N", "N", "N", "Y"))
#'
#' code_threeway(df, pid_self, pid_actl)
#' code_threeway(df, issue_self, issue_actl)
#'
#'
code_threeway <- function(data, var1, var2) {
  var1 <- enquo(var1)
  var2 <- enquo(var2)

  data %>%
    mutate(out = case_when(
      ((!!var1 %in% c("D", "R") & !!var2 %in% c("D", "R")) |
        (!!var1 %in% c("Y", "N") & !!var2 %in% c("Y", "N"))) &  !!var1 == !!var2 ~ 1,
      ((!!var1 %in% c("D", "R") & !!var2 %in% c("D", "R")) |
         (!!var1 %in% c("Y", "N") & !!var2 %in% c("Y", "N"))) &  !!var1 != !!var2 ~ -1,
      TRUE ~ 0
    )) %>%
    mutate(out = replace(out, !!var1 %in% c("DK", "I"), 0)) %>%
    mutate(out = replace(out, is.na(!!var1) | is.na(!!var2), NA)) %>%
    pull(out)
}

