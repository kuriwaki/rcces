#' Recode CCES variable values so that they merge to ACS variables
#'
#' @param tbl A subset of the cumulative common content. Must include variables
#'  \code{age}, \code{race}, \code{educ}, and \code{gender}.
#'
#' @export
#'
ccc_std_demographics <- function(tbl) {
  tbl %>%
    # geography
    mutate(cd = as.character(glue("{st}-{str_pad(dist, width = 2, pad = '0')}"))) %>%
    # age
    mutate(age_bin = rcces::ccc_bin_age(age)) %>%
    # race
    mutate(race = as.character(as_factor(race))) %>%
    rename(race_cces_chr = race) %>%
    left_join(distinct(race_key, race_cces_chr, race), by = "race_cces_chr") %>%
    # education
    rename(educ_cces = educ) %>%
    mutate(educ_cces = as.character(as_factor(educ_cces))) %>%
    left_join(distinct(select(educ_key, educ_cces, educ)), by = "educ_cces") %>%
    # sort
    select_if(~any(!is.na(.x))) %>%
    select(matches("year"),
           matches("case_id"),
           matches("weight"),
           matches("(state|st|cd|dist)"),
           matches("gender"),
           matches("pid3$"),
           matches("age"),
           matches("educ"),
           matches("^race"),
           matches("faminc"),
           matches("marstat"),
           matches("citizen"),
           matches("vv"),
           everything()) %>%
    distinct()
}
