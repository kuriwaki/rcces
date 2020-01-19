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



#' Discretize a vector of age integers into labelled variables
#'
#' @param agevec a vector of integers
#' @param agelbl a value-key pair to be passed into recode,
#'  with values as the things to be recoded and labels as the labels for each
#'  value.
#'
#' @importFrom tibble deframe
#' @examples
#'   ccc_bin_age(c(15:100, NA))
#'
#' @export
ccc_bin_age <- function(agevec,
                        agelbl = deframe(age5_key)) {
  int_bin <- case_when(agevec %in% 18:24 ~ 1L,
                       agevec %in% 25:34 ~ 2L,
                       agevec %in% 35:44 ~ 3L,
                       agevec %in% 45:64 ~ 4L,
                       agevec >=   65    ~ 5L,
                       TRUE ~ NA_integer_)

  haven::labelled(int_bin, agelbl)
}
