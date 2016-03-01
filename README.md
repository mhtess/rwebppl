# RWebPPL

### Sketch of plan

+ change WebPPL binary
+ `util.serialize` output in `topK` and `printWebPPLValue`
+ unserialize using `jsonlite` in R

~~~
wppl_model <- "var data =  5
var n = 10

var model = function(){
	var theta = beta(1,1)

	var score = binomialERP.score([theta,n], data);

	factor(score)

	return {"theta":theta}
}

var results = MH(model, 5000)"


rwebppl <- function(model_file, outputs) {
	model <- read(model_file)
	outputs_model <- model + sprintf("print(ouputs)")
	command <- sprintf("webppl %s", outputs_model)
	result <- system(command, intern = TRUE)
	do_stuff(result)
}
~~~