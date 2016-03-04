tidy_output <- function(model_output) {
  if (!is.null(names(model_output)) &&
      all(names(model_output) == c("probs", "support"))) {
    data.frame(support = model_output$support,
               probs = model_output$probs)
  } else {
    model_output
  }
}

#' webppl
#'
#' Runs webppl model.
#'
#' @param model_code A string of a webppl program.
#' @param model_file A file containing a webppl program.
#'
#' @return The model's return value(s).
#' @export
#'
#' @examples
#' model_code <- "flip(0.5)"
#' webppl(model_code)
webppl <- function(model_code = NULL, model_file = NULL) {
  if (!is.null(model_code)) {
    cat(model_code, file = (f <- tempfile()))
  } else if (!is.null(model_file) && file.exists(model_file)) {
    f <- model_file
  } else {
    stop("no model file or model code supplied")
  }
  js_path <- system.file("js", package = "rwebppl")
  script_path <- file.path(js_path, "rwebppl")
  output_string <- paste(system2(script_path, args = c(f), stdout = TRUE),
                         collapse = "")
  tidy_output(jsonlite::fromJSON(output_string))
}
