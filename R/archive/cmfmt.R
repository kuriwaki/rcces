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
