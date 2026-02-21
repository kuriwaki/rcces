#' Recode H/S
#'
#' A convenience function to recode H to House (or CDs) and S to Senate
#'
#' @param x A character vector of chamber codes (\code{"H"}, \code{"S"})
#' @param h_to Label for House
#' @param s_to Label for Senate
#'
#' @importFrom dplyr case_match
#' @examples
#' H_to_house(c("H", "S"))
#' H_to_cd(c("H", "S"))
#' H_to_rep(c("H", "S"))
#'
#' @export
H_to_house <- function(x) {
  case_match(
    x,
    "H" ~ "House",
    "S" ~ "Senate",
  )
}

#' @rdname H_to_house
#' @export
H_to_cd <- function(x, h_to = "Congressional Districts", s_to = "States") {
  case_match(
    x,
    "H" ~ h_to,
    "S" ~ s_to,
  )
}

#' @rdname H_to_house
#' @export
H_to_rep <- function(x, h_to = "House Representative", s_to = "Senator") {
  case_match(
    x,
    "H" ~ h_to,
    "S" ~ s_to,
  )
}

#' Standardize party codes to single capital letters
#'
#' @param x A vector of characters with unstandardized party, e.g. "Republican", "Not sure"..
#'
#' @return A vector of "D", "R",
#'
#' @importFrom dplyr recode
#'
#' @examples
#' std_short_party(
#'  c("Republican",
#'    "Democrat",
#'    "Not sure",
#'    "Other Party / Independent",
#'    "Never Heard of Person")
#'    )
#'
#'
#' @export
#'
std_short_party <- function(x) {
  dplyr::recode(x,
         Democrat = "D",
         Republican = "R",
         Independent = "I",
         `Never Heard of Person` = "NeverHeard",
         `Not sure` = "NotSure",
         `Not Sure` = "NotSure",
         `Don't know` = "NotSure",
         `Other Party / Independent` = "OtherI",
         `Other Party/Independent` = "OtherI",
         `Skipped` = "",
         `skipped` = "",
         `Not Asked` = ""
  )
}

