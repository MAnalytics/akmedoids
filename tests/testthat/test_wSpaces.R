context("Tests for checking data types for wSpaces function");

#simulating different whitespaces
sn <- c('sn1','sn2','sn3')
col1 <- c(21000, 23400, 26800)
col2 <- as.character(c("23 ","45 ","23 ")) #trailing whitespaces
col3 <- as.character(c(" 23"," 45", " 23")) #leading whitespaces
col4 <- as.character(c(" 23 ", " 45 ", " 23 ")) #trailing and leading

test.data1 <- data.frame(sn, col1, col2, col3, col4)

test_that('data types are correct', {
  expect_is(traj,'data.frame')
  expect_is(colnames(traj), 'character')
  #expect_is(testing_data$letters, 'character') #this one fails; they're factors
})



#hh=wSpaces(test.data1)

#hh[2,2]+1
