context("Testing statPrint function");


# #2.checking that no data are missing..
test_that('no missing values', {
    expect_identical(clustr, na.omit(clustr))
 })

#
test_that('cluster label matches traj length', {
  expect_equal(length(clustr), 10)
  expect_is(clustr, 'character')

})


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


#generate cluster stats
clust1 <- as.vector(clust1$memberships)
stats = statPrint(clust1, df, id_field=TRUE,
                  type="lines", y.scaling="fixed", showplots=FALSE)

gr1 <- as.numeric(as.character(stats$descriptiveStats$n))[1]
gr2 <- as.numeric(as.character(stats$descriptiveStats$n))[2]
gr3 <- as.numeric(as.character(stats$descriptiveStats$n))[3]

#test increment of clust
incr = function(a, b){
  if(a>=b){
    check = 1}
  else{
    check = 2
  }
  return(check)
}

st1 <- as.numeric(as.character(stats$changeStats$`%+ve Traj.`))[1]
st2 <- as.numeric(as.character(stats$changeStats$`%+ve Traj.`))[2]
st3 <- as.numeric(as.character(stats$changeStats$`%+ve Traj.`))[3]

test_that("test cluster stats routine is okay", {
  expect_equal(gr1, 3)
  expect_equal(gr2, 1)
  expect_equal(gr3, 1)

  expect_equal(incr(st2, st1), 1)
  expect_equal(incr(st3, st1), 1)
  expect_equal(incr(st3, st2), 1)

})

