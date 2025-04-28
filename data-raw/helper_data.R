# Get the full hierarchy of tables and categories on the BSI website
full <- get_all_bsi_tables()

################################################################################
# rerun this after you make any changes here!!!
################################################################################
usethis::use_data(full,
                  internal = FALSE,
                  overwrite = TRUE)
