context("clustering test")

df <- data.frame(id=c("u","v","w","x","y"),
                 a=c(2, 3, 5, 2, 0),
                 b=c(5, 1, 0, 3, 1),
                 c=c(5, 1, 4, 2, 7))

expct_clust <- c("C", "A", "A", "A", "B")

clust1 <- akmedoids.clust(df, k = c(3))
clust2 <- akmedoids.clust(df, k = c(3,5), crit="Calinski_Harabasz")

test_that("test clustr procedures and the output specification", {
  expect_error(akmedoids.clust(df, k = c(2)))
  expect_error(akmedoids.clust(df, k = c(dim(df)[1]-4, 4)))
  expect_error(akmedoids.clust(df, k = c(3, dim(df)[1]+2)))

  #outputs
  expect_identical(as.vector(clust1$memberships), expct_clust)
  expect_equal(length(unique(as.vector(clust1$memberships))), 3)
  #plot is made

})


#simulate a set of pre-defined clusters


