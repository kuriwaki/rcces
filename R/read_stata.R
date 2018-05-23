
#' Recreate Stata's varaiable table
#'
#' @param dta output from \env{read_dta}
#' @param string string to search and filter, optional
#'
#' @export
#' @examples
#' # example data (what a read_dta tbl would look like)
#' df <- tibble(id = 1:3, pid = 1:3, age = 1:3)
#' attr(df[["id"]], "label") <- "Case Identifier"
#' attr(df[["pid"]], "label") <- "Partisan Identity"
#'
#' vartab(df)
#' vartab(df, "identity")
vartab <- function(dta, string = NULL) {

  # Get the label from variable alias
  get_label <- function(name, df = dta) {
    lab <- attr(dta[[name]], "label", exact = TRUE)
    if (is.null(lab)) return(NA)
    if (!is.null(lab)) return(lab)
  }

  # vartable
  vt <- tibble(name = names(dta),
               label = map_chr(names(dta), get_label)
  )

  if (is.null(string)) return(vt)

  # filter search if provided a string
  if (!is.null(string)) {
    vt %>%
      filter(str_detect(name, regex(string, ignore_case = TRUE)) |
               str_detect(label, regex(string, ignore_case = TRUE)))
  }
}
