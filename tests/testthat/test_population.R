context("Testing population.R function")

test_that('data types, dimension of test data', {
  expect_is(population,'data.frame')
  expect_is(colnames(population), 'character')
  expect_equal(nrow(population), 11)
  expect_equal(ncol(population), 3)
})

test_that('id_field complete and unique', {
  expect_equal(length(unique(population$location_id)), 11)
  expect_identical(population$location_id,
                   na.omit(population$location_id))
})


