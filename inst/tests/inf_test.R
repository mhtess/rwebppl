myModel <- "
var model = function(){
  var a = uniform(0,1)
  condition(a>0.3)
  return a
}
"

inference_opts <- list(
  method = "MCMC",
  samples = 10000,
  burn = 50,
  thin = 5,
  verbose = TRUE
)

single <- webppl(model_code = myModel, model_var = "model",
                 inference_opts = inference_opts)

multi <- webppl(model_code = myModel, model_var = "model",
                inference_opts = inference_opts,
                chains = 4, cores = 4)

ggmulti <- webppl(model_code = myModel, model_var = "model",
                inference_opts = inference_opts, ggmcmc = TRUE,
                chains = 4, cores = 4)
