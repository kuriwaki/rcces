
#' Recreate Stata's variable table
#'
#' @param dta output from \env{read_dta} or \env{read_sav}
#' @param string string to search and filter, optional
#' @param name Name of the variable for the column of original stata variables
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect str_c
#' @importFrom tibble tibble
#' @export
#' @examples
#' # example data (what a read_dta tbl would look like)
#' df <- tibble(id = 1:3, pid = 1:3, age = 1:3)
#' attr(df[["id"]], "label") <- "Case Identifier"
#' attr(df[["pid"]], "label") <- "Partisan Identity"
#'
#' vartab(df)
#' vartab(df, "identity")
vartab <- function(dta, string = NULL, name = alias) {
  name <- enquo(name)
  namevar <- quo_name(name)

  # Get the label from variable alias
  get_label <- function(name, df = dta) {
    lab <- attr(dta[[name]], "label", exact = TRUE)
    if (is.null(lab)) return(NA)
    if (!is.null(lab)) return(lab)
  }

  # apply get_label in tibble
  vt <- tibble(i = 1:ncol(dta),
               name = names(dta),
               label = map_chr(names(dta), get_label),
               class = map_chr(dta, ~str_c(class(.x),  collapse = ", "))) %>%
    rename(!!namevar := name)

  if (is.null(string)) return(vt)

  # filter search if provided a string
  if (!is.null(string)) {
    vt %>%
      filter(str_detect(!!name, regex(string, ignore_case = TRUE)) |
               str_detect(label, regex(string, ignore_case = TRUE)))
  }
}

#' Append variable labels for Stata export
#'
#' @param tbl A dataset
#' @param labels A set of levels. Either a tibble that has
#'  two columns called `alias` and `description`, OR a vector
#'  of the form `c(alias1 = "description1", alias2 = "description2)` that
#'  can be coerced into one.
#' @export
#'
#' @examples
#'  mt_stata <- as_tibble(mtcars) |>
#'   attach_varlab(c(cyl = "Number of cylinders", hp = "Gross horsepower"))
#'
#'  # bulk edits by a tibble format
#'  vartab <- tribble(
#'   ~ alias, ~description,
#'   "cyl",  "Number of cylinders",
#'   "hp",  "Gross horsepower"
#'  )
#'  mt_stata <- as_tibble(mtcars) |>
#'   attach_varlab(vartab)
#'
#'
attach_varlab <- function(tbl, labels, overwrite = TRUE) {

  if (!inherits(labels, "data.frame"))
    labs <- tibble::enframe(labels, name = "alias", value = "description")
  else
    labs <- labels

  # for each variable chr in the dataset
  for (v in colnames(tbl)) {
    match_descr <- labs$description[which(labs$alias == v)]
    curr_lab <- attributes(tbl[[v]])$label

    # if there is a match
    if (length(match_descr) != 0) {
      # if there is currently no label, write
      if (is.null(curr_lab) || curr_lab == "")
        attributes(tbl[[v]])$label <- match_descr

      # if a label already exists, only change if overwrite=TRUE
      if (!is.null(curr_lab) && curr_lab != "" && isTRUE(overwrite))
        attributes(tbl[[v]])$label <- match_descr

    } else {
      # if none exist, leave blank
      if (is.null(curr_lab))
        attributes(tbl[[v]])$label <- ""
    }
  }

  tbl
}
