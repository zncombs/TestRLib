
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

expect_that(make_filename(2017),is_a('character'))

