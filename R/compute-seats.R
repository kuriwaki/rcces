#' Compute measure for seats relevant for seats-votes
#' @param se_factor What to multiply the standard errors by after.
#' @import dplyr
#' @export
#'
#' @examples
#' library(tibble)
#' dist_opinion <- tibble(
#'   q_label      = rep(c("Gun Background Check 2018", "Repeal ACA 2017"), each = 5),
#'   pct_yes2_raw = c(0.55, 0.48, 0.62, 0.51, 0.44,
#'                    0.70, 0.65, 0.58, 0.72, 0.61),
#'   n_raw        = rep(200, 10)
#' )
#' prop_seats(dist_opinion)
prop_seats <- function(
    tbl,
    pct_var = pct_yes2_raw,
    n_var = n_raw,
    group_var = q_label,
    se_factor = 1) {
  tbl |>
    mutate(sdest = se_factor*sqrt({{pct_var}}*(1 - {{pct_var}})/{{n_var}})) |>
    summarize(
      prop_dists_supp_dir = mean({{pct_var}} > 0.5),
      prop_dists_supp = mean(pnorm((0.5 - {{pct_var}})/(sdest), lower.tail = FALSE)),
      avg_dist_supp = mean({{pct_var}}),
      sd_dists_supp = sd({{pct_var}}),
      avg_se = mean(sdest),
      avg_n = mean({{n_var}}),
      n_dists = n(),
      .by = {{group_var}})
}
