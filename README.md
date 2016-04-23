# RWebPPL

[![Travis-CI Build Status](https://travis-ci.org/mhtess/rwebppl.svg?branch=master)](https://travis-ci.org/mhtess/rwebppl)

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

## Passing data from R to WebPPL

Data can be passed from R to WebPPL as in:

```
webppl(my_model, 
	   data = df, 
	   data_var = "myDF")
```

The data can be accessed in your WebPPL model under the name `myDF`. 

If `df` looks like this in R:

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

## R helper functions

WebPPL ERPs are automatically turned into histograms (values with probabilities associated with it). To recover samples from an ERP, you can use something like the following: 

```
# expecting the final column to the the probability
histToSamples <- function(df, samples){
  df[rep(row.names(df), df[,tail(names(df),1)]*(samples)), 1:ncol(df)-1]
}
```
