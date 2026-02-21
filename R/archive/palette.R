#' Red and Blue colors for D vs. R character
#' @export
#'
#' @seealso ggredist::scale_color_party_d()
#' @examples
#' library(tibble)
#' library(ggplot2)
#'
#' tibble(party = c("D", "R")) |>
#'  ggplot(aes(color = party)) +
#'  geom_text(aes(label = party), x = 0.5, y = 0.5, size = 12) +
#'  facet_wrap(~ party) +
#'  scale_color_DR()
#'
scale_color_DR <- function (labels = c(R = "Rep.", D = "Dem."), reverse = FALSE, ...)  {
  pal = c(R = "#B25D4C", D = "#3D77BB")
  if (reverse)
    pal = rev(pal)
  ggplot2::scale_color_manual(..., values = pal, labels = labels)
}


#' @rdname scale_color_DR
#' @export
scale_fill_DR <- function (labels = c(R = "Rep.", D = "Dem."), reverse = FALSE, ...)  {
  pal = c(R = "#B25D4C", D = "#3D77BB")
  if (reverse)
    pal = rev(pal)
  ggplot2::scale_fill_manual(..., values = pal, labels = labels)
}

