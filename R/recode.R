#' Standardize party codes to single capital letters
#'
#' @param x A vector of characters with unstandardized party, e.g. "Republican", "Not sure"..
#'
#' @return A vector of "D", "R",
#'
#' @examples
#' std_short_party(c("Republican", "Democrat", "Not sure", "Other Party / Independent", "Never Heard of Person")
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

