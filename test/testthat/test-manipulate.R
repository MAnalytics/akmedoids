context("manipulation functions")

df <- data.frame(id=c("u","v","w","x","y"),
                 a=c(2, 3, 5, 2, 0),
                 b=c(5, 1, 0, 3, 1),
                 c=c(5, 1, 4, 2, 7))

df_props = props(df)
sub_df_props = df_props[ ,2:ncol(df_props)]
sum_df_props = sum(colSums(sub_df_props))

expected_1 = sum(rep(1, 3))

test_that("column sum of proportions is equal to one, throw warnings", {
  expect_is(df_props, "data.frame")
  expect_equal(sum_df_props, expected_1, tolerance=1e-2)
  expect_warning(props(df, id_field = FALSE))
})


df2 <- data.frame(a=c(2, NA, 5, 2, 0),
                 b=c(5, 1, Inf, 3, 1),
                 c=c(5, 1, 4, 2, 4))

output_df2 = dataImputation(df2, id_field = FALSE)

test_that("check data imput recognises different missing entries", {
  expect_equal(output_df2[2,1], 1)
  expect_equal(output_df2[3,2], 4.5)
})



clust_ids <- c(1, 3, 2, 2, 1, 23, 2, 3)
expect_clust_alpha = c("A", "C", "B", "B", "A", "W", "B", "C")
clust_ids_char <- c(1, 3, 2, "2", 1, 23, 2, 3)

test_that("input & output need be vector of numbers and characters, respct. ", {
  expect_identical(alphaLabel(clust_ids), expect_clust_alpha)
  expect_is(alphaLabel(clust_ids), "character")
  #expect_equal(alphaLabel(clust_ids_char), rep(NA, 8))
})

#alphaLabel(expect_clust_alpha)
