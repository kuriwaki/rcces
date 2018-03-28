
#' Parse no-space labels to cleaner words for graphing
#'
#' @param char text to parse
#'
#' @export
my.parse <- function(char, drop_year = TRUE) {
  pattern <- "([a-z])([A-X2])"
  replacement <- "\\1 \\2"

  char <- gsub(pattern, replacement, char,  perl = T, ignore.case = F)
  char <- gsub("ACA2", "ACA 2", char)
  char <- gsub("SCHIP2", "SCHIP 2", char)
  if (drop_year) char <- gsub("(20[0-1][1-9])", "", char,  perl = T, ignore.case = F)

  return(char)
}

#' comma formatting to char
#' @rdname cmfmt
#' @param x numeric vector
#' @param file path to write to
#' @param round round by
#' @param pp if true then multiply by one hundred
#' @export
#'
cmfmtW <- function(x, file, round = NA, pp = FALSE) {
  if (pp) x <- round(x*100, 1)
  if (!is.na(round)) x <- round(x, round)

  x <- format(x, big.mark = ",")
  writeLines(x, file)
}

#' @rdname cmfmt
#' @param x numeric vector
#' @param pp if true then multiply by one hundred
#' @export
#'
#' @examples
#' cmfmt(c(123, 1234, 123456789))
#'
cmfmt <- function(x, file,  pp = FALSE) {
  if (pp) x <- round(x*100, 1)
  x <- format(x, big.mark = ",")
  return(x)
}

clabs <- c(`109` = "109th Congress (Bush, Hastert, Frist)",
           `110` = "110th Congress (Bush, Pelosi, Reid)",
           `111` = "111th Congress (Obama, Pelosi, Reid)",
           `112` = "112nd Congress (Obama, Boehner, Reid)",
           `113` = "113rd Congress (Obama, Boehner, Reid)",
           `114` = "114th Congress (Obama, Boehner/Ryan, McConnell)",
           `115` = "115th Congress (Trump, Ryan, McConnell)")


clabs.graph <- c("109\nBush\nHastert\nFrist",
                 "110\nBush\nPelosi\nReid",
                 "111\nObama\nPelosi\nReid",
                 "112\nObama\nBoehner\nReid",
                 "113\nObama\nBoehner\nReid",
                 "114\nObama\nBoehner-Ryan\nMcConnell")


congEra <- data_frame(cong = 109:114,
                      name = clabs.graph,
                      start = c(2006.0, 2006.2, 2008.2, 2010.2, 2012.2, 2014.2),
                      end   = c(2006.2, 2008.2, 2010.2, 2012.2, 2014.2, 2016.2),
                      name.x = c(2006, 2007.2, 2009.2, 2011.2, 2013.2, 2015.2),
                      name.y = rep(0.6, 6),
                      type  = "",
                      pty4  = "foo")

cong.color <- c(`109` = "orangered3",
                `110` = "royalblue3",
                `111` = "royalblue3",
                `112` = "purple3",
                `113` = "purple3",
                `114` = "orangered3")
