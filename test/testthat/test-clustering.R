context("clustering and stats test")

df <- data.frame(id=c("u","v","w","x","y"),
                 a=c(2, 3, 5, 2, 0),
                 b=c(5, 1, 0, 3, 1),
                 c=c(5, 1, 4, 2, 7))

df_non_unique <- data.frame(id=c("u","u","w","x","y"),
                 a=c(2, 3, 5, 2, 0),
                 b=c(5, 1, 0, 3, 1),
                 c=c(5, 1, 4, 2, 7))

expct_clust <- c("C", "A", "A", "A", "B")

clust1 <- akmedoids.clust(df, k = c(3))

test_that("test clustr procedures and the output specification", {
  expect_error(akmedoids.clust(df, k = c(2)))
  expect_error(akmedoids.clust(df, k = c(dim(df)[1]-4, 4)))
  expect_error(akmedoids.clust(df, k = c(3, dim(df)[1]+2)))
  expect_error(akmedoids.clust(df, k = c(5, 4)))
  expect_error(akmedoids.clust(df_non_unique, id_field=TRUE, k = c(3)))#non-unique 'id-field'
  #outputs
  expect_identical(as.vector(clust1$memberships), expct_clust)
  expect_equal(length(unique(as.vector(clust1$memberships))), 3)

})


clustr <- akmedoids.clust(df, id_field = TRUE,
                          k = 3)
clustr <- as.vector(clustr$memberships)
print(statPrint(clustr, df, id_field=TRUE, type="lines", y.scaling="fixed"))

test_that("test cluster stats routine is okay", {


})






