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

.onAttach <- function(libname, pkgname) {
  pkg_path <- system.file(package = "rwebppl")
  if (!file.exists(file.path(pkg_path, "package.json"))) {
    packageStartupMessage("webppl not found, installing...")
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
#' @param data A data frame (or other serializable object) that can be referenced in the model.
#' @param data_var A name by which data can be referenced in the model.
#' @param model_packages A character vector of external package names to use.
#'
#' @return The model's return value(s).
#' @export
#'
#' @examples
#' model_code <- "flip(0.5)"
#' webppl(model_code)
webppl <- function(model_code = NULL, model_file = NULL, data = NULL,
                   data_var = NULL, model_packages = NULL) {
  packages <- model_packages
  if (!is.null(data) && !is.null(data_var)) {
    tmp_dir <- tempdir()
    dir.create(file.path(tmp_dir, data_var), showWarnings = FALSE)
    cat(sprintf('{"name":"%s","main":"index.js"}', data_var),
        file = file.path(tmp_dir, data_var, "package.json"))
    data_string <- jsonlite::toJSON(data)
    cat(sprintf("module.exports = JSON.parse('%s')", data_string),
        file = file.path(tmp_dir, data_var, "index.js"))
    packages <- c(packages, file.path(tmp_dir, data_var))
  }
  if (!is.null(model_code)) {
    cat(model_code, file = (f <- tempfile()))
  } else if (!is.null(model_file) && file.exists(model_file)) {
    f <- model_file
  } else {
    stop("no model file or model code supplied")
  }
  if (!is.null(packages)) {
    package_args <- unlist(lapply(packages,
                                  function(x) paste("--require", x)))
  } else {
    package_args <- ""
  }
  script_path <- system.file("js/rwebppl", package = "rwebppl")
  output_file = "/tmp/webppl_output"
  if (file.exists(output_file)) {
    file.remove(output_file)
  }
  system2(script_path, args = c(f, package_args), stdout = "", wait = FALSE)
  while (!file.exists(output_file)) {Sys.sleep(1)}
  output_string <- paste(readLines(output_file), collapse = "\n")
  if (output_string != "") {
    tidy_output(jsonlite::fromJSON(output_string))
  }
}
