library(rwebppl)
context("Passing data")

test_that("list passing from r to webppl and back", {
	expect_equal(webppl("data",
             data = list(a = 5, b = 7),
             data_var = "data"),
		list(a = 5, b = 7))
	})