context("Creating Tables")

test_that("table works with one variable", {
  expect_type(create_table(iris, Sepal.Width), type = "list")
})
