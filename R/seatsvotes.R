#' Conversion of proportions to log of odds
#'
#' @param x Numeric vector of proportions
#' @param clip Logical; if \code{TRUE}, clip values to avoid 0 and 1
#' @param clip_by Numeric offset from boundaries when clipping
#'
#' @return Log odds transformation of \code{x}
#'
#' @keywords internal
logodds <- function(x, clip = TRUE, clip_by = 0) {
  if (clip)
    x <- pmin(pmax((0 + clip_by), x), (1 - clip_by))

  return(log(x / (1 - x)))
}


#' Seats-votes curve fitting
#'
#' Evaluate a seats-votes curve at given vote shares, using a log-odds
#' (logit) transformation parameterized by bias (intercept) and
#' responsiveness (slope).
#'
#' @param x Numeric vector of vote proportions at which to evaluate the curve
#' @param intercept The intercept at 50-50 votes. The deviation from 50
#'   percent is also referred to as bias.
#' @param slope The slope on the log odds scale. Also referred to as
#'   responsiveness.
#' @param model An OLS model with log odds as the y and x variables. The
#'   first coefficient is used as the intercept and the second as the slope.
#'   If provided, \code{intercept} and \code{slope} are ignored.
#'
#' @return A numeric vector of seat proportions
#'
#' @export
#'
#' @examples
#' votes <- seq(0.1, 0.9, by = 0.05)
#' seats_mj <- sv_curve(votes, intercept = 0, slope = 3)
#' seats_PR <- sv_curve(votes, intercept = 0, slope = 1)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   sv_df <- data.frame(votes = votes, seats_PR = seats_PR, seats_mj = seats_mj)
#'   ggplot(sv_df, aes(x = votes)) +
#'     geom_line(aes(y = seats_PR), color = "navy") +
#'     geom_line(aes(y = seats_mj), color = "forestgreen") +
#'     coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
#'     labs(x = "Votes", y = "Seats")
#' }
sv_curve <- function(x, intercept, slope, model = NULL) {
  if (!is.null(model)) {
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
  }

  logodds_y <- intercept + slope * log(x / (1 - x))
  return(exp(logodds_y) / (1 + exp(logodds_y)))
}
