# Create a small sample of 2014 CCES common content for package examples.

library(dataverse)
library(haven)
library(dplyr)

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

cc14_raw <- get_dataframe_by_name(
  filename  = "CCES14_Common_Content_Validated.tab",
  dataset   = "doi:10.7910/DVN/XFXJVY",
  original  = TRUE,
  .f        = read_dta
)

set.seed(02138)
cces14_sample <- cc14_raw |>
  slice_sample(n = 100) |>
  mutate(year = 2014L) |>
  select(
    case_id = V101,
    year,
    CC14_325_1, CC14_325_2, CC14_325_3, CC14_325_4, CC14_325_5,
    CC14_320a, CC14_320b, CC14_320c
  )

usethis::use_data(cces14_sample, overwrite = TRUE)
