# RWebPPL

## Installation

To use rwebppl, you need to have [nodejs](https://nodejs.org/en/) installed.

You can install rwebppl from GitHub using devtools:

```
devtools::install_github("mhtess/rwebppl")
```

## Usage

Write a model as a string in R:

```
my_model <- "
 var a = flip(0.3)
 var b = flip(0.6)
 return a + b
"
webppl(my_model)
```

Or write a model in an external file:

```
webppl(model_file = "path/to/model/model.wppl")
```

You can also use WebPPL packages in more complex models:

```
webppl(model_file = "path/to/model/model.wppl",
       model_packages = c("projectUtils", "helpers"))
```
