library(rwebppl)
context("Basic WebPPL call")

test_that("webppl call returns data frame", {
	expect_true(is.data.frame(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} )
			factor(Binomial( {p : 0.5, n: 5 } ).score(4) )
			return {p: p}
		}
		Infer({method:"rejection", samples:1000}, model)
		')))
})