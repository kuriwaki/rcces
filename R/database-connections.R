
#' @export
#'
library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)

con <- dbConnect(SQLite(), "~/Dropbox/CCES_representation/data/database/db.sqlite3")
initExtension(con)
src <- src_dbi(con)

respn <- tbl(src, "responses")
agrmt <- tbl(src, "agreement")
person <- tbl(src, "person")
legis <- tbl(src, "legis")
votes <- tbl(src, "votes")
qID <- tbl(src, "qID")
qID_df <- collect(qID)
distr <- tbl(src, "distr")
splts <- tbl(src, "splits")

