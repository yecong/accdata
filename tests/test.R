Sys.setenv("R_TESTS" = "")
library(testthat)
source("/Users/yidancong/Desktop/AccData/R/fars_functions.R")

expect_that(fars_read("accident_1994.csv.bz2"),
            throws_error("file 'accident_1994.csv.bz2' does not exist"))
expect_that(fars_read_years({"1993"}), gives_warning("invalid year: 1993"))
expect_that(make_filename("2015"), equals("accident_2015.csv.bz2"))
