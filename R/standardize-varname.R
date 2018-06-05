#' Standardize name of party perception variable across different CCES datasets
#'
#' Relies on having the office
#'
#' @param df CCES dataframe with multiple "perceived vote" questions. Slimmed.
#' @param prefix character that recogizes the perceived vote question
#'
#' @export
#'
#' @return   The same df but with variables renamed with suffixes.
std_party_prcp_varname <- function(df, prefix =  "party_prcp_") {

  vt <- vartab(df)

  # get alias of matching
  rep_var <-   filter(vt, str_detect(label, "(Rep|House)")) %>% pull(alias)
  if (length(rep_var) > 1) {
    rep_var <- filter(vt, str_detect(label, "(House)")) %>% pull(alias)
  }
  sen1_var <-  filter(vt, str_detect(label, "Sen(ator\\s|)1")) %>% pull(alias)
  sen2_var <-  filter(vt, str_detect(label, "Sen(ator\\s|)2")) %>% pull(alias)
  gov_var <-   filter(vt, str_detect(label, "Gov(ernor|)")) %>% pull(alias)

  if (any(length(rep_var) != 1,
          length(sen1_var) != 1,
          length(sen2_var) != 1,
          length(gov_var) != 1))  {
    stop ("Choose better regex to pinpoint column")
  }

  rep_new  <- str_c(prefix, "rep")
  sen1_new <- str_c(prefix, "sen1")
  sen2_new <- str_c(prefix, "sen2")
  gov_new  <- str_c(prefix, "gov")

  df %>%
    rename(!!rep_new := !!rep_var,
           !!sen1_new := !!sen1_var,
           !!sen2_new := !!sen2_var,
           !!gov_new := !!gov_var) %>%
    mutate_at(vars(matches("prcp")), function(x) as.character(as_factor(x)))
}
