# RWebPPL

[![Travis-CI Build Status](https://travis-ci.org/mhtess/rwebppl.svg?branch=master)](https://travis-ci.org/mhtess/rwebppl)

RWebPPL is an R package providing an interface to [WebPPL](https://github.com/probmods/webppl), a probabilistic programming language.

## Installation

#### System requirements
+ Mac or Linux OS [Windows currently not supported]
+ [R v3.3](https://cran.cnr.berkeley.edu) (in RStudio, type `version`)
+ [node](https://nodejs.org/en/) v4.4.5 or higher (in Terminal, type `node --version`; close and re-open terminal after install)
+ [npm](https://docs.npmjs.com/getting-started/installing-node) v3.6 or higher (in Terminal, type `npm --version`; if it's not >= v3.6; try `sudo npm install npm -g`, close and reopen terminal and check version)
+ devtools R library: in R: `install.packages('devtools')`

```
devtools::install_github("mhtess/rwebppl")
```

RWebPPL always installs its own local version of WebPPL for stability: by default, it will install the most recent, compatible release. Advanced users may use the `install_webppl()` function to override this default to install from any official NPM release tag (e.g. '0.9.7') or any commit hash from the WebPPL github repository.

## Primary features

#### Running models from R

Write a model as a string in R:

```
my_model <- '
   var model = function () {
      var a = flip(0.3)
      var b = flip(0.6)
      return a + b
   }
   model()
'
webppl(my_model)
```

Or write a model in an external file:

```
webppl(program_file = "path/to/model/model.wppl")
```

#### Passing data to WebPPL from R

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

#### Running multiple chains (in parallel)

```
webppl(my_model, chains = 3, cores = 3)
```

#### Other options

- specifying inference options in R
- setting a random seed in R

For a complete introduction to RWebPPL's functionality, see [this introduction](https://github.com/mhtess/rwebppl/wiki/Introduction). 
