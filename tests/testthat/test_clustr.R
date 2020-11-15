context("Testing clustr.R function")

test_that('data types, dimension of test data', {
  expect_is(clustr,'character')
  expect_equal(length(clustr), 9)
})
