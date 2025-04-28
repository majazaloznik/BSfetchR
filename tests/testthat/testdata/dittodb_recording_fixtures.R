# This code was run once and is here for archival purposes.
#
source("tests/testthat/helper-connection.R")

start_db_capturing()
con <- make_connection()
out <- prepare_category_table("i_36_6as", con = con)
stop_db_capturing()


start_db_capturing()
con <- make_connection()
out <- prepare_source_table(con = con)
con_test <- make_test_connection()
out <- prepare_source_table(con = con_test, schema = "test_platform")
stop_db_capturing()
