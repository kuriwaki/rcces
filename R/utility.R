## utlity functions -------
my.parse <- function(char, drop_year = TRUE) {
  pattern <- "([a-z])([A-X2])"
  replacement <- "\\1 \\2"

  char <- gsub(pattern, replacement, char,  perl = T, ignore.case = F)
  char <- gsub("ACA2", "ACA 2", char)
  char <- gsub("SCHIP2", "SCHIP 2", char)
  if (drop_year) char <- gsub("(20[0-1][1-9])", "", char,  perl = T, ignore.case = F)

  return(char)
}

# comma formatting
cmfmtW <- function(x, file, round = NA, pp = F) {
  if (pp) x <- round(x*100, 1)
  if (!is.na(round)) x <- round(x, round)

  x <- format(x, big.mark = ",")
  writeLines(x, file)
}

cmfmt <- function(x, file,  pp = F) {
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
