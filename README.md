# RWebPPL

## Getting started

Get package

```
library(devtools)
install_github("mhtess/rwebppl")
```

Install webppl
```
library(rwebppl)
install_webppl()
```

### Using webppl 

Write model in RStudio

```
model <- "
 var a = flip(0.3)
 var b = flip(0.6)
 return a + b
"
webppl(model)
```

Write model in external file

```
webppl(model_file = "path/to/model/model.wppl")
```

Use WebPPL packages in more complex models

```
webppl(model_file = "path/to/model/model.wppl",
	model_packages = c("projectUtils", "helpers"))
```
