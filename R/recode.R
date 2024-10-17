#' Recode H/S
#'
#' A convenience function to recode H to House (or CDs) and S to Senate
#'
#' @importFrom dplyr case_match
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(ggplot2)
#'
#'
#' tibble(chamber = c("H", "S")) |>
#'  ggplot() +
#'  geom_text(aes(label = chamber), x = 0.5, y = 0.5) +
#'  facet_wrap(~ chamber, labeller = labeller(chamber = H_to_house))
#'
#' tibble(chamber = c("H", "S")) |>
#'  ggplot() +
#'  geom_text(aes(label = chamber), x = 0.5, y = 0.5) +
#'  facet_wrap(~ chamber, labeller = labeller(chamber = H_to_cd))
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

