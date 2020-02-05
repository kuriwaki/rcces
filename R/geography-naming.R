#' get CD format from ACS
#'
#' @param vec a vector of strings from the ACS congressional district naming
#'
#' @examples
#' cd_from_acs("Congressional District 32 (115th Congress), California")
#'
#' @export
cd_from_acs <- function(vec) {

  st_to_state <- tibble(st = state.abb, state = state.name) %>%
    add_row(st = "DC", state = "District of Columbia")

  distnum <- vec %>%
    str_extract("([0-9]+|at Large)") %>%
    str_replace("at Large", "1") %>%
    str_pad(width = 2, pad = "0")
  cong <- vec %>% str_extract("1[01][0-9]")
  states <- vec %>% str_extract("(?<=,\\s)[A-z\\s]+")
  st <- map_chr(states, function(x) st_to_state$st[x == st_to_state$state])

  return(as.character(glue("{st}-{distnum}")))
}
