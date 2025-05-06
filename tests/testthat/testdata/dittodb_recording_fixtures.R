# This code was run once and is here for archival purposes.
#
source("tests/testthat/helper-connection.R")

#
# start_db_capturing()
# con <- make_connection()
# out <- prepare_source_table(con = con)
# con_test <- make_test_connection()
# out <- prepare_source_table(con = con_test, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_connection()
# result1 <- get_px_list("i_36_6as")
# result2 <- get_px_metadata("i_36_6as", con)
# result3 <- get_px_dim_levels("i_36_6as", con)
# result4 <- get_px_data("i_36_6as", con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_connection()
# out <- prepare_category_table("i_36_6as", con = con)
# stop_db_capturing()
#
# start_db_capturing()
# con <- make_connection()
# prepare_table_table("i_36_6as", con = con)
# stop_db_capturing()

# start_db_capturing()
# con <- make_connection()
# prepare_category_relationship_table("i_36_6as", con)
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# # table_table <- prepare_table_table("i_36_6as", con = con_test, schema = "test_platform")
# # UMARimportR::insert_new_table_table(con_test, table_table, schema = "test_platform")
# prepare_category_table_table("i_36_6as", con_test, "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# prepare_table_dimensions_table("F2_Q1S", con_test, "test_platform")
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# out <- prepare_dimension_levels_table("I1_1S", con_test, "test_platform")
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# out <- prepare_series_table("I1_1S", con_test, "test_platform")
# stop_db_capturing()
#
#
# start_db_capturing()
# con_test <- make_test_connection()
# out <- prepare_series_levels_table("I1_1S", con_test, "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# con_test <- make_test_connection()
# result <- BS_import_structure("F2_Q1S", con_test, schema = "test_platform")
# stop_db_capturing()


start_db_capturing()
con_test <- make_test_connection()
x <- prepare_vintage_table("I1_1S", con_test, schema = "test_platform")
expect_true(all(names(x) == c("series_id", "published")))
stop_db_capturing()
