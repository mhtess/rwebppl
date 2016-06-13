# RWebPPL

[![Travis-CI Build Status](https://travis-ci.org/mhtess/rwebppl.svg?branch=master)](https://travis-ci.org/mhtess/rwebppl)

## Installation

To use rwebppl, you need to have [`nodejs`](https://nodejs.org/en/) installed, as well as [`npm`](https://www.npmjs.com/) of version `>=3.8.0`.

You can install rwebppl from GitHub using devtools:

```
devtools::install_github("mhtess/rwebppl")
```

If WebPPL is not automatically installed, it can be installed used `install_webppl()`.

## Usage

### Current list of function arguments and supported functionality

+ `program_code`: A string of a webppl program 
+ `program_file`: A file containing a webppl program
+ `data`: A data frame (or other serializable object) to be passed from R to the webppl program
+ `data_var`: A name by which the data can be referenced in the webppl program
+ `packages`: A character vector of names of external webppl package to use
+ `model_var`: [*experimental*] When using inference opts, the name by which the model be referenced in the program.
+ `inference_opts`: [*experimental*] A list with options for inference of a particular model in the program. (see http://webppl.readthedocs.io/en/master/inference.html) 
+ `chains`: [*experimental*] Number of times to run program (defaults to 1).
+ `cores`: [*experimental*] Number of cores to use when running multiple chains (defaults to 1).
+ `ggmcmc`: [*experimental*] Logical indicating whether to transform output into ggmcmc-ready format.

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
