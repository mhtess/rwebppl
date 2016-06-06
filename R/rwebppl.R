# Path to rwebppl R package
rwebppl_path <- function() system.file(package = "rwebppl")

# Path to webppl npm package
webppl_path <- function() path.expand("~/.rwebppl")

# Path to where webppl looks for webppl npm packages
global_pkg_path <- function() path.expand("~/.webppl")

#' Install webppl
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{install_webppl()}
install_webppl <- function() {
  system2(file.path(rwebppl_path(), "bash", "install.sh"),
          args = c(webppl_path(), rwebppl_path()))
}

.onAttach <- function(libname, pkgname) {
  if (!file.exists(file.path(webppl_path(), "node_modules", "webppl"))) {
    packageStartupMessage("webppl not found, installing...")
    install_webppl()
  }
}

#' Install an npm package for webppl
#'
#' @param package_name Name of package to be installed
#' @param path Path to package install location (defaults to webppl's global
#'   package directory)
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{install_webppl_package("babyparse")}
install_webppl_package <- function(package_name, path = global_pkg_path()) {
  system2(file.path(rwebppl_path(), "bash", "install_package.sh"),
          args = c(path, package_name, rwebppl_path()))
}

#' Uninstall an npm package for webppl
#'
#' @inheritParams install_webppl_package
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{uninstall_webppl_package("babyparse")}
uninstall_webppl_package <- function(package_name, path = global_pkg_path()) {
  system2(file.path(rwebppl_path(), "bash", "uninstall_package.sh"),
          args = c(path, package_name))
}

tidy_output <- function(model_output) {
  if (!is.null(names(model_output)) &&
      length(names(model_output)) == 2 &&
      all(names(model_output) %in% c("probs", "support"))) {
    if (class(model_output$support) == "data.frame") {
      support <- model_output$support
    } else {
      support <- data.frame(support = model_output$support)
    }
    cbind(support, data.frame(prob = model_output$probs))
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
  #script_path <- file.path(pkg_path(), "js", "rwebppl")
  script_path <- file.path(webppl_path(), "rwebppl")
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

  # set output_arg to path to temporary file with a unique key
  output_arg <- sprintf("/tmp/webppl_output_%s",
                        digest::digest(as.character(Sys.time()), algo = "md5",
                                       serialize = FALSE))

  # create --require argument out of each package name
  if (!is.null(packages)) {
    package_args <- unlist(lapply(packages,
                                  function(x) paste("--require", x)))
  } else {
    package_args <- ""
  }

  # clear paths where rwebppl JS script will write output or errors
  #output_file = "/tmp/webppl_output"
  output_file <- output_arg
  if (file.exists(output_file)) {
    file.remove(output_file)
  }
  error_file <- "/tmp/webppl_error"
  if (file.exists(error_file)) {
    file.remove(error_file)
  }

  # run rwebppl JS script with model file and packages as arguments
  # any output to stdout gets sent to the R console while command runs
  system2(script_path, args = c(file_arg, output_arg, package_args),
          stdout = "", stderr = error_file, wait = FALSE)

  # wait for output file or error file to exist
  while (!(file.exists(output_file) ||
           (file.exists(error_file) && file.info(error_file)$size != 0))) {
    Sys.sleep(1)
  }

  # if the command produced an error, raise the error
  if (file.exists(error_file) && file.info(error_file)$size != 0) {
    stop(paste(readLines(error_file), collapse = "\n"))
  }

  # if the command produced output, collect and tidy the results
  if (file.exists(output_file)) {
    output_string <- paste(readLines(output_file), collapse = "\n")
    if (output_string != "") {
      return(tidy_output(jsonlite::fromJSON(output_string, flatten = TRUE)))
    }
  }
}
