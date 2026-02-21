
#' Parse compact question labels to readable words for graphing
#'
#' Inserts spaces into camelCase question/issue labels and optionally
#' strips year suffixes, e.g. \code{"CapAndTrade"} becomes \code{"Cap And Trade"}.
#'
#' @param char text to parse
#' @param drop_year If \code{TRUE} (default), remove four-digit year strings
#'
#' @export
#' @examples
#' # Real CCES question labels (camelCase from the question-vote key)
#' parse_qlabel(c("BanAssaultRifle2013", "RepealACA2017", "CapAndTrade"))
#' # [1] "Ban Assault Rifle " "Repeal ACA "  "Cap And Trade"
#'
#' # Keep year suffixes
#' parse_qlabel(c("GunBackgroundCheck2018", "RaiseMinimumWage2016"), drop_year = FALSE)
#' # [1] "Gun Background Check 2018" "Raise Minimum Wage 2016"
#'
#' # Handles acronyms and special cases
#' parse_qlabel(c("SCHIP2009", "PPACA", "DoddFrank"), drop_year = FALSE)
#' # [1] "SCHIP 2009" "PPACA" "Dodd Frank"
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
#' @examples
#' # Write a formatted count to a .tex file for LaTeX inclusion
#' cmfmtW(123456, tempfile(fileext = ".tex"))
#'
#' # Write a rounded percentage
#' cmfmtW(0.6712, tempfile(fileext = ".tex"), round = 1, pp = TRUE)
cmfmtW <- function(x, file, round = NA, pp = FALSE) {
  if (pp) x <- round(x*100, 1)
  if (!is.na(round)) x <- round(x, round)

  x <- format(x, big.mark = ",")
  purrr::walk2(x, file, ~ write_lines(.x, .y))
}

