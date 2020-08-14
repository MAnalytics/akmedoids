context("Tests for data types");

test_that('data types are correct', {
  expect_is(traj,'data.frame')
  expect_is(colnames(traj), 'character')
})

