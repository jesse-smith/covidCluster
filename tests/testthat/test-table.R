context("Creating Tables")

test_that("table works with 1 variable (output is expected size)", {
  expect_equal(dim(create_table(iris, Species)), expected = c(3, 3))
})

test_that("table works with >1 variables (output is expected size)", {
  expect_equal(
    dim(create_table(iris, dplyr::starts_with("Sepal"))),
    expected = c(57, 3)
  )
})

test_that("table sums to expected total with multiple variables", {
  expect_equal(
    {
      table <- create_table(iris, dplyr::starts_with("Sepal"))
      sum(table$n)
    },
    label = "sum(table$n)",
    expected = NROW(iris)
  )
})
