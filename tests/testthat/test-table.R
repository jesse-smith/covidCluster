context("Creating Tables")

test_that("create_table works with 1 variable (output is expected size)", {
  expect_equal(dim(create_table(iris, Species)), expected = c(3, 3))
})

test_that("create_table works with >1 variables (output is expected size)", {
  expect_equal(
    dim(create_table(iris, dplyr::starts_with("Sepal"))),
    expected = c(57, 3)
  )
})
