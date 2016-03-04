model_code <- "
  [flip(0.5), flip(0.3)]
"

webppl <- function(model_code = NULL, model_file = NULL) {
  #assertthat::validate_that((!is.null(model_file) && file.exists(model_file)) | !is.null(model_code))
  if (!is.null(model_code)) {
    cat(model_code, file = (f <- tempfile()))
  } else if (!is.null(model_file) && file.exists(model_file)) {
    f <- model_file
  } else {
    stop("no model file or model code supplied")
  }
  output_string <- system2("./exec/rwebppl", args = c(f), stdout = TRUE)
  output <- jsonlite::fromJSON(output_string)
  if (all(names(output) == c("probs", "support"))) {
    data.frame(support = output$support,
               probs = output$probs)
  } else {
    output
  }
}
