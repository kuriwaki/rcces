
#' Compute measure for seats relevant for seats-votes
#' @import dplyr
#' @export
prop_seats <- function(tbl, pct_var = pct_yes2_raw, n_var = n_raw, group_var = q_label) {
  tbl |>
    mutate(sdest = sqrt({{pct_var}}*(1 - {{pct_var}})/{{n_var}})) |>
    summarize(
      prop_dists_supp_dir = mean({{pct_var}} > 0.5),
      prop_dists_supp = mean(pnorm((0.5 - {{pct_var}})/(2*sdest), lower.tail = FALSE)),
      avg_dist_supp = mean({{pct_var}}),
      sd_dists_supp = sd({{pct_var}}),
      n_dists = n(),
      .by = {{group_var}})
}
