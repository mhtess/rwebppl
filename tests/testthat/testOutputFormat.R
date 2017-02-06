library(rwebppl)
context("Output format")

test_that("enumerate returns probability table", {
	expect_true(all(names(webppl('
		var model = function() {
			var p = uniformDraw([0.2,0.5,0.8,0.99] )
      var q = uniformDraw([0.2,0.5,0.8,0.99] )
      var theta = p*q;
			observe(Binomial( {p : theta, n: 5 } ), 4)
      return {p, q}
		}',
    inference_opts = list(method = "enumerate"),
    model_var = "model")) == c("p", "q", "prob")))
})

test_that("rejection returns df w correct rownames", {
	expect_true(all(names(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} );
      var q = uniform( {a:0, b:1} );
      var theta = p*q;
			observe(Binomial( {p : theta, n: 5 } ), 4)
			return {p, q}
		}',
    inference_opts = list(method = "rejection", samples = 100),
    model_var = "model")) == c("p", "q")))
})

test_that("MCMC returns df w correct rownames", {
	expect_true(all(names(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} );
      var q = uniform( {a:0, b:1} );
      var theta = p*q;
			observe(Binomial( {p : theta, n: 5 } ), 4)
			return {p, q}
      }',
      inference_opts = list(method = "MCMC", samples = 100),
      model_var = "model")) == c("p", "q")))
})

test_that("incrementalMH returns df w correct rownames", {
	expect_true(all(names(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} );
      var q = uniform( {a:0, b:1} );
      var theta = p*q;
			observe(Binomial( {p : theta, n: 5 } ), 4)
			return {p, q}
      }',
    inference_opts = list(method = "incrementalMH", samples = 100),
    model_var = "model")) == c("p", "q")))
})

test_that("forward returns df w correct rownames", {
	expect_true(all(names(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} );
      var q = uniform( {a:0, b:1} );
      var theta = p*q;
			return {p, q}
      }',
    inference_opts = list(method = "forward", samples = 100),
    model_var = "model")) == c("p", "q")))
})

test_that("SMC returns df w correct rownames", {
	expect_true(all(names(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} );
      var q = uniform( {a:0, b:1} );
      var theta = p*q;
			observe(Binomial( {p : theta, n: 5 } ), 4)
			return {p, q}
    }',
    inference_opts = list(method = "SMC", particles = 100),
    model_var = "model")) == c("p", "q")))
})

test_that("infer optimize returns df w correct rownames", {
	expect_true(all(names(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} );
      var q = uniform( {a:0, b:1} );
      var theta = p*q;
			observe(Binomial( {p : theta, n: 5 } ), 4)
			return {p, q}
    }',
    inference_opts = list(method = "optimize", samples = 100),
    model_var = "model")) == c("p", "q")))
})

test_that("optimize returns tidied df", {
	expect_true(all(c("param", "dims", "length") %in% names(webppl('
		var model = function() {
			var p = uniform( {a:0, b:1} );
      var q = uniform( {a:0, b:1} );
      var theta = p*q;
			observe(Binomial( {p : theta, n: 5 } ), 4)
			return {p, q}
    };
    Optimize({model:model, steps: 100})')) ))
})
