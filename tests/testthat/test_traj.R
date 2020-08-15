context("Tests for data types");

test_that('data types, dimension of test data', {
  expect_is(traj,'data.frame')
  expect_is(colnames(traj), 'character')
  expect_equal(nrow(traj), 10)
  expect_equal(ncol(traj), 10)
})

