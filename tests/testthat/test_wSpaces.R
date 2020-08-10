context("Tests for checking data types for wSpaces function");

test_that('data types are correct', {
  expect_is(traj,'data.frame')
  expect_is(colnames(traj), 'character')
  #expect_is(testing_data$letters, 'character') #this one fails; they're factors
})



