#' Install webppl
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{install_webppl()}
install_webppl <- function() {
  pkg_path <- system.file(package = "rwebppl")
  system(sprintf("cd %s && \
                 npm init -y . &>/dev/null && \
                 npm install --save webppl &>/dev/null &&\
                 cd node_modules/webppl &&\
                 npm install &>/dev/null",
                 pkg_path),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
}

.onLoad <- function(libname, pkgname) {
  pkg_path <- system.file(package = "rwebppl")
  if (!file.exists(file.path(pkg_path, "package.json"))) {
    cat("webppl not found, installing...")
    install_webppl()
  }
}

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
#' @param model_packages A character vector of external package names to use.
#'
#' @return The model's return value(s).
#' @export
#'
#' @examples
#' model_code <- "flip(0.5)"
#' webppl(model_code)
webppl <- function(model_code = NULL, model_file = NULL,
                   model_packages = NULL) {
  if (!is.null(model_code)) {
    cat(model_code, file = (f <- tempfile()))
  } else if (!is.null(model_file) && file.exists(model_file)) {
    f <- model_file
  } else {
    stop("no model file or model code supplied")
  }
  if (!is.null(model_packages)) {
    package_args <- unlist(lapply(model_packages,
                                  function(x) paste("--require", x)))
  } else {
    package_args <- ""
  }
  script_path <- system.file("js/rwebppl", package = "rwebppl")
  output_string <- paste(
    system2(script_path, args = c(f, package_args), stdout = TRUE),
    collapse = ""
  )
  if (output_string == "") {
    ""
  } else {
    tidy_output(jsonlite::fromJSON(output_string))
  }
}
