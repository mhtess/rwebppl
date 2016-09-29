# RWebPPL

[![Travis-CI Build Status](https://travis-ci.org/mhtess/rwebppl.svg?branch=master)](https://travis-ci.org/mhtess/rwebppl)

## Installation


#### System requirements
+ Mac or Linux OS [Windows currently not supported]
+ [R v3.3](https://cran.cnr.berkeley.edu) (in RStudio, type `version`)
+ [node](https://nodejs.org/en/) v4.4.5 or higher (in Terminal, type `node --version`; close and re-open terminal after install)
+ [npm](https://docs.npmjs.com/getting-started/installing-node) v3.6 or higher (in Terminal, type `npm --version`; if it's not >= v3.6; try `sudo npm install npm -g`, close and reopen terminal and check version)
+ devtools R library: in R: `install.packages('devtools')`

N.B.: If you plan to use command-line WebPPL, in addition to RWebPPL, we advise you first [`npm install -g webppl`](http://webppl.readthedocs.io/en/master/quickstart.html) and then install RWebPPL.

N.B.: If your node set-up depends on your environment variables, you may need to start rstudio from the terminal (rstudio does not otherwise load `.bashrc` files, for example).

```
devtools::install_github("mhtess/rwebppl")
```

If you already have installed WebPPL using `npm install -g webppl`, then RWebPPL will link to your installed version.

If you do not have WebPPL installed already, it will install it for you (into the R package directory).


## Usage

### Current list of function arguments and supported functionality

+ `program_code`: A string of a webppl program 
+ `program_file`: A file containing a webppl program
+ `data`: A data frame (or other serializable object) to be passed from R to the webppl program
+ `data_var`: A name by which the data can be referenced in the webppl program
+ `packages`: A character vector of names of external webppl package to use
+ `model_var`: When using inference opts, the name by which the model be referenced in the program.
+ `inference_opts`: A list with options for inference of a particular model in the program. (see http://webppl.readthedocs.io/en/master/inference.html) [N.B.: requires using `model_var`]
+ `output_format`: An optional string indicating posterior output format: "webppl" probability table (default), "samples" for just the samples, "ggmcmc" for use with [ggmcmc package](http://xavier-fim.net/packages/ggmcmc/). [N.B.: requires using `inference_opts` and `model_var`]
+ `chains`: Number of times to run program (defaults to 1).
+ `cores`: Number of cores to use when running multiple chains (defaults to 1).


### Examples

Write a model as a string in R:

```
my_model <- "
var model = function () {
 var a = flip(0.3)
 var b = flip(0.6)
 return a + b
}
model()
"
webppl(my_model)
```

Or write a model in an external file:

```
webppl(program_file = "path/to/model/model.wppl")
```

[WebPPL packages](http://webppl.readthedocs.io/en/master/packages.html) can be used in more complex models:

```
webppl(program_file = "path/to/model/model.wppl",
       packages = c("linked-list", "timeit"))
```

[List of useful packages.](https://github.com/probmods/webppl/wiki/Useful-packages)

NPM packages that ares used inside of WebPPL packages can be installed directly from RWebPPL e.g. `install_webppl_package("babyparse")`. They can also be uninstalled in the same way: `uninstall_webppl_package("babyparse")`

## Passing data from R to WebPPL

Data can be passed directly from R to WebPPL as in:

```
my_model <- "
var model = function () {
 var a = flip(0.3)
 var b = flip(0.6)
 var scores = map( function(d) {
 	return a + b - d
 }, myDF)
 return scores
}
model()
"

webppl(my_model,
	   data = df,
	   data_var = "myDF")
```

In this example, `myDF` is not defined inside the WebPPL program, but is passed into it from R, using `data = df`. The argument `data_var` tells WebPPL what the data should be called.

### Structure of data when passing

If `myDF` looks like this in R:

| Participant | Condition | Response |
|-------------|-----------|----------|
| 1           | A         | 0.4      |
| 1           | B         | 0.8      |
| 2           | A         | 0.2      |

It will exist in WebPPL as a list of js objects e.g.

```
[
  {
    participant: 0,
    condition: "A",
    response: 0.4
  },
  {
    participant: 0,
    condition: "B",
    response: 0.8
  },
  {
    participant: 1,
    condition: "A",
    response: 0.2
  },
  ...
]
```
