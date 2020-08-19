context("Testing statPrint.R function");

set.seed(12)
#sample trajectories
df <- data.frame(id=c("1","2","3","4","5","6","7","8","9","10"),
                 t1=sample(0:10, 10, replace = TRUE),
                 t2=sample(0:10, 10, replace = TRUE),
                 t3=sample(0:10, 10, replace = TRUE),
                 t4=sample(0:10, 10, replace = TRUE),
                 t5=sample(0:10, 10, replace = TRUE),
                 t6=sample(0:10, 10, replace = TRUE),
                 t7=sample(0:10, 10, replace = TRUE))
df

#non_unique id field
df2 <- df
df2$id[3] <- 2

clusterng <- akmedoids.clust(df, k = c(3))

#generate cluster stats
clust_list <- as.vector(clusterng$memberships)$alphabetic_Labels

#derive cluster stats
statistics = statPrint(clust_list, df, showplots=FALSE)

test_that('check error msgs output correctly', {
  #non-matching lengths
  expect_error(statPrint(cluster_list[1:9], df, showplots=FALSE, N.quant = 2,
       prints_text(paste("*----Unequal number of clusters",
                         "elements and trajectories----*", sep=" "))))
  #invalid quantile value
  expect_error(statPrint(cluster_list, df, showplots=FALSE, N.quant = 1,
       prints_text(paste("*----Please, enter an integer between 2",
       "and 10 for the 'N.quant' argument'!!!----*", sep=" "))))

  #invalid quantile value
  expect_error(statPrint(cluster_list, df, showplots=FALSE, N.quant = 11,
       prints_text(paste("*----Please, enter an integer between 2",
       "and 10 for the 'N.quant' argument'!!!----*", sep=" "))))

  #invalid quantile value
  expect_error(statPrint(cluster_list, df, showplots=FALSE, N.quant = 2,
       prints_text(paste("(: The 'id_field' is not a unique field.",
                         "Function terminated!!! :)", sep=" "))))

})

test_that('output clusters counts are accurate', {
   expect_equal(sum(as.numeric(statistics$descriptiveStats$n)), 10)
   expect_equal(sum(as.numeric(statistics$descriptiveStats$`n(%)`)), 100)
   expect_equal(sum(as.numeric(statistics$changeStats$`%+ve Traj.`[1]),
                    as.numeric(statistics$changeStats$`%-ve Traj.`[1])), 100)
   expect_equal(sum(as.numeric(statistics$changeStats$`%+ve Traj.`[2]),
                    as.numeric(statistics$changeStats$`%-ve Traj.`[2])), 100)
   expect_equal(sum(as.numeric(statistics$changeStats$`%+ve Traj.`[3]),
                    as.numeric(statistics$changeStats$`%-ve Traj.`[3])), 100)
})

#ordering of clusters
incr = function(a, b){
  if(a>=b){
    check = 1}
  else{
    check = 2
  }
  return(check)
}

test_that('testing cluster categories', {
  expect_equal(incr(as.numeric(statistics$changeStats$`%-ve Traj.`[1]),
                  as.numeric(statistics$changeStats$`%+ve Traj.`[1])), 1)
  expect_equal(incr(as.numeric(statistics$changeStats$`%+ve Traj.`[3]),
                    as.numeric(statistics$changeStats$`%-ve Traj.`[3])), 1)

})


