
#' Parse compact question labels to readable words for graphing
#'
#' Inserts spaces into camelCase question/issue labels and optionally
#' strips year suffixes, e.g. \code{"gunControl2012"} becomes \code{"gun Control"}.
#'
#' @param char text to parse
#' @param drop_year If \code{TRUE} (default), remove four-digit year strings
#'
#' @export
parse_qlabel <- function(char, drop_year = TRUE) {
  pattern <- "([a-z])([A-X2])"
  replacement <- "\\1 \\2"

  char <- gsub(pattern, replacement, char,  perl = T, ignore.case = F)
  char <- gsub("ACA2", "ACA 2", char)
  char <- gsub("SCHIP2", "SCHIP 2", char)
  if (drop_year) char <- gsub("(20[0-1][1-9])", "", char,  perl = T, ignore.case = F)

  return(char)
}

#' @rdname parse_qlabel
#' @export
my.parse <- function(char, drop_year = TRUE) {
  .Deprecated("parse_qlabel")
  parse_qlabel(char, drop_year = drop_year)
}

#' comma formatting to char
#' @rdname cmfmt
#' @param x numeric vector
#' @param file path to write to
#' @param round round by
#' @param pp if true then multiply by one hundred
#'
#' @importFrom readr write_lines
#' @importFrom purrr walk2
#' @export
#'
cmfmtW <- function(x, file, round = NA, pp = FALSE) {
  if (pp) x <- round(x*100, 1)
  if (!is.na(round)) x <- round(x, round)

  x <- format(x, big.mark = ",")
  purrr::walk2(x, file, ~ write_lines(.x, .y))
}

