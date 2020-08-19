context("Testing akmedoids.clust.R function");

#simulate dataset
sn <- c('sn1','sn2','sn3', 'sn4', 'sn5')

set.seed(5)
col1 <- sample(1:10, 5)
col2 <- sample(1:10, 5)
col3 <- sample(1:10, 5)
col4 <- sample(1:10, 5)
col5 <- sample(1:10, 5)
col6 <- sample(1:10, 5)

test.data8 <- data.frame(sn, col1, col2,
                         col3, col4, col5, col6)

#sample with with non-unique id_field
test.data9 <- test.data8
test.data9$sn[3] <- "sn2"

test_that('print the right message post clustering', {
  expect_that(akmedoids.clust(test.data8, id_field = TRUE, k = c(3)),
              prints_text("solution of k = 3 determined!"))
})
context("Testing akmedoids.clust.R function");

test_that('check error msgs output correctly', {
  #cluster number cannot be less than 3
  expect_error(akmedoids.clust(test.data8, id_field = TRUE, k = c(2),
         prints_text("(: Program terminated!!! :)")))
  #number of clusters cannot descend
  expect_error(akmedoids.clust(test.data8, id_field = TRUE, k = c(4:3),
         prints_text("(: Program terminated!!! :)")))
  #id_field non_unique
  expect_error(akmedoids.clust(test.data9, id_field = TRUE, k = c(3),
         prints_text(paste("(: The 'id_field' does not contain unique",
         "elements. Function terminated!!! :)", sep=" "))))
  expect_error(akmedoids.clust(test.data8, id_field = TRUE, k = c(3:5),
         crit="someRandomCrit",
         prints_text(paste("*----*(: Quality criterion specified is NOT",
         "RECOGNISED!! Execution terminated!!! :)*----*", sep= " "))))
  expect_error(akmedoids.clust(test.data8, id_field = TRUE, k = c(3:5),
         crit="Silhouette",
         prints_text(paste("*----*(: 'Silhouette' criterion is not applicable!.",
         "Try 'Calinski_Harabasz':)*----*", sep=" "))))
})


output = akmedoids.clust(test.data8, id_field = TRUE, k = c(3:5),
                         crit = "Calinski_Harabasz")

test_that('check that output is complete', {
  expect_equal(length(output), 4)
  expect_equal(length(output$qualitycriterion), 1)
  expect_equal(length(output$membership_optimalSolution), 1)
  expect_equal(length(output$membership_optimalSolution$alphabetic_Labels), 5)
  expect_equal(nrow(output$qualityCrit.List), 2)
  expect_equal(ncol(output$qualityCrit.List), 2)
})


#checking that cluster labels are complete.
test_that('no missing label', {
    expect_identical(output$membership_optimalSolution$alphabetic_Labels,
              na.omit(output$membership_optimalSolution$alphabetic_Labels))
 })


test_that('cluster label matches traj size', {
  expect_equal(length(output$membership_optimalSolution$alphabetic_Labels), 5)
})

test_that('data type are correct', {
  expect_is(output$qualitycriterion, 'character')
  expect_is(output$membership_optimalSolution$alphabetic_Labels, 'character')
  expect_is(output$qualityCrit.List, 'data.frame')
})

