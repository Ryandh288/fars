# testing depends on 3 files in the test directory
ls()

# test make_filename
expect_identical(make_filename(2013),
                 'accident_2013.csv.bz2')

# test fars_read

test_that('test fars_read', {
  # error when file does not exist
  expect_error(fars_read('accident_1900.csv.bz2'),
               "file 'accident_1900.csv.bz2' does not exist")

  # run when exist; check class and dimensions
  dat <- fars_read('accident_2013.csv.bz2')
  expect_identical(class(dat), c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(dat), c(30202, 50))
})

system.file('extdata', 'accident_2013.csv.bz2', package = 'fars')

# test fars_read_years
test_that('test fars_read_years', {
  # warning when a year does not exist
  yrs <- 2013:2016
  expect_warning(fars_read_years(yrs),
                 "invalid year: 2016")

  # run; check class and length
  yrs <- 2013:2015
  dat <- fars_read_years(yrs)
  expect_identical(class(dat), 'list')
  expect_identical(length(dat), length(yrs))
})
# fars_summarize_years
test_that('test fars_summarize_years', {
  #run; test class and dimensions
  yrs <- 2013:2015
  dat <- fars_summarize_years(yrs)
  expect_identical(class(dat), c("tbl_df", "tbl", "data.frame"))
  expect_equal(dim(dat), c(12, 4))
})

# fars_map_state
test_that('test fars_map_state', {
  # error when invalid state number
  expect_error(fars_map_state(1000, 2013),
               'invalid STATE number: 1000')
})
