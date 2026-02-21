#' simpler version of dyad agreement, intended for long form where office is set
#'
#' @param data dataset
#' @param var1 first variable of Y, N, DK
#'
#' @importFrom dplyr mutate case_when pull
#'
#' @export
yes_no_agrmt <- function(var1, var2, data) {
  var1 <- enquo(var1)
  var2 <- enquo(var2)

  data %>%
    mutate(out = case_when(
      !!var1 == "Y" & !!var2 == "Y" ~ 1,
      !!var1 == "N" & !!var2 == "N" ~ 1,
      !!var1 == "Y" & !!var2 == "N" ~ -1,
      !!var1 == "N" & !!var2 == "Y" ~ -1,
      !!var1 == "DK" | !!var2 == "DK" ~ 0,
      TRUE ~ 0
    )) %>%
    mutate(out = replace(out, is.na(!!var1) | is.na(!!var2), NA)) %>%
    pull(out)
}
