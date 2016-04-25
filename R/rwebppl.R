#' Install webppl
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{install_webppl()}
install_webppl <- function() {
  pkg_path <- system.file(package = "rwebppl")
  system(sprintf("%s/install.sh %s", pkg_path, pkg_path))
}

.onAttach <- function(libname, pkgname) {
  pkg_path <- system.file(package = "rwebppl")
  if (!file.exists(file.path(pkg_path, "node_modules"))) {
    packageStartupMessage("webppl not found, installing...")
    install_webppl()
  }
}

tidy_output <- function(model_output) {
  if (!is.null(names(model_output)) &&
      length(names(model_output)) == 2 &&
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
#' @param data A data frame (or other serializable object) that can be
#'   referenced in the model.
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

  # find location of rwebppl JS script, within rwebppl R package
  script_path <- system.file("js/rwebppl", package = "rwebppl")
  packages <- model_packages

  # if data supplied, create a webppl package that exports the data as data_var
  if (!is.null(data)) {
    if (is.null(data_var)) {
      warning("ignoring data (supplied without data_var)")
    } else {
      tmp_dir <- tempdir()
      dir.create(file.path(tmp_dir, data_var), showWarnings = FALSE)
      cat(sprintf('{"name":"%s","main":"index.js"}', data_var),
          file = file.path(tmp_dir, data_var, "package.json"))
      data_string <- jsonlite::toJSON(data)
      cat(sprintf("module.exports = JSON.parse('%s')", data_string),
          file = file.path(tmp_dir, data_var, "index.js"))
      packages <- c(packages, file.path(tmp_dir, data_var))
    }
  }

  # set file_arg to temporary file containing model_code or to mode_file
  if (!is.null(model_code)) {
    if (!is.null(model_file)) {
      warning("both model_code and model_file supplied, using model_code")
    }
    cat(model_code, file = (file_arg <- tempfile()))
  } else if (!is.null(model_file)) {
    if (!file.exists(model_file)) {
      stop("model_file does not exist")
    }
    file_arg <- model_file
  } else {
    stop("supply one of model_code or model_file")
  }

  # create --require argument out of each package name
  if (!is.null(packages)) {
    package_args <- unlist(lapply(packages,
                                  function(x) paste("--require", x)))
  } else {
    package_args <- ""
  }

  # clear path where rwebppl JS script will write output
  output_file = "/tmp/webppl_output"
  if (file.exists(output_file)) {
    file.remove(output_file)
  }

  # run rwebppl JS script with model file and packages as arguments
  # any output to stdout gets sent to the R console while command runs
  system2(script_path, args = c(file_arg, package_args),
          stdout = "", wait = FALSE)

  # wait for output file to exist, then collect and tidy results
  while (!file.exists(output_file)) {Sys.sleep(1)}
  output_string <- paste(readLines(output_file), collapse = "\n")
  if (output_string != "") {
    tidy_output(jsonlite::fromJSON(output_string))
  }
}
