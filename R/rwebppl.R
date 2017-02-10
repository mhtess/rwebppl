# Path to rwebppl R package
rwebppl_path <- function() system.file(package = "rwebppl")

# Path to local webppl install
webppl_path = function() paste(c(rwebppl_path(), "js", "webppl", "webppl"), collapse = "/")

# Path to where webppl looks for webppl npm packages
global_pkg_path <- function() path.expand("~/.webppl")

# Internal function that checks whether a file exists
file_exists <- function(path) {
  args <- c("!", "-e", path, ";", "echo", "$?")
  existsFlag <- suppressWarnings(system2("test", args = args, stdout = T))
  return(existsFlag == 1)
}

#' Installs webppl locally
#' 
#' Supports both official npm release versions (e.g. '0.9.6') and 
#' also commit hashes from the github repository for custom configurations
#' @param webppl_version official npm tag or commit hash
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{install_webppl('0.9.6')}
#' \dontrun{install_webppl('4bd2452333d24c122aee98c3206584bc39c6096a')}
install_webppl <- function(webppl_version) {
  message("installing webppl ...", appendLF = FALSE)
  npm_info <- system2("npm", args = c("info", "webppl", "versions", "--json"),
                      stdout = TRUE)
  npm_versions <- jsonlite::fromJSON(paste(npm_info, collapse = ""))
  if (webppl_version %in% npm_versions) {
    rwebppl_json <- file.path(rwebppl_path(), "json", "rwebppl.json")
    rwebppl_meta <- jsonlite::fromJSON(readLines(rwebppl_json))
    rwebppl_meta$dependencies$webppl <- webppl_version
    webppl_json <- file.path(rwebppl_path(), "js", "package.json")
    writeLines(jsonlite::toJSON(rwebppl_meta, auto_unbox = TRUE, pretty = TRUE),
               webppl_json)
    system2(file.path(rwebppl_path(), "bash", "install-webppl.sh"),
            args = rwebppl_path())
  } else {
    system2(file.path(rwebppl_path(), "bash", "install-dev-webppl.sh"),
            args = c(rwebppl_path(), webppl_version))
  }
  system2(file.path(rwebppl_path(), "bash", "rearrange-webppl.sh"),
          args = rwebppl_path())
  message(" done")
}

# Internal function to ensure the user already has webppl installed on load
# Installs default version in DESCRIPTION if it doesn't already exist
check_webppl <- function() {
  if (!file_exists(webppl_path())) {
    webppl_version <- utils::packageDescription("rwebppl", fields = "WebPPLVersion")
    install_webppl(webppl_version)
  }
}

#' Prints out version of webppl
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{get_webppl_version()}
get_webppl_version <- function() {
  if (file_exists(webppl_path())) {
    version_str <- system2(webppl_path(), args = c("--version"), stdout = T)
    message(paste("local webppl exists:", version_str))
  } else {
    warning("couldn't find local webppl install")
  }
}

.onLoad <- function(libname, pkgname) {
  check_webppl()
  get_webppl_version()
}

#' Install webppl package
#'
#' Install an npm package to webppl's global installation.
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

#' Uninstall webppl package
#'
#' Uninstall an npm package from webppl's global installation.
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

#' Get samples
#'
#' Turn webppl "histogram" output into samples.
#'
#' @param df A data frame of webppl "histogram" output (has a column called
#'   `prob` with probabilities, remaining columns are parameter values).
#' @param num_samples A number of samples to reconstruct.
#' @return Data frame of parameter values with number of rows equal to
#'   `num_samples`.
#' @export
#'
#' @examples
#' num_samples <- 10
#' df <- data.frame(prob = c(0.1, 0.3, 0.5, 0.1), support = c("a","b","c","d"))
#' get_samples(df, num_samples)
get_samples <- function(df, num_samples) {
  rows <- rep.int(seq_len(nrow(df)), times = round(df$prob * num_samples))
  cols <- names(df) != "prob"
  df[rows, cols, drop = FALSE]
}

tidy_output <- function(model_output, output_format = "webppl", chains = NULL,
                        chain = NULL, inference_opts = NULL) {
  if (!is.null(names(model_output)) &&
      length(names(model_output)) == 2) {
    if (all(names(model_output) %in% c("probs", "support"))) {
      if (class(model_output$support) == "data.frame") {
        support <- model_output$support
      } else {
        support <- data.frame(support = model_output$support)
      }
      tidied_output <- cbind(support, data.frame(prob = model_output$probs))
    } else if ("score" %in% names(model_output)) {
      tidied_output <- model_output[, names(model_output) != "score",
                                    drop = FALSE]
    } else {
      tidied_output <- model_output
    }
    if (output_format=="ggmcmc" & !is.null(inference_opts) & !is.null(chain) &
        !is.null(chains)) {
      num_samples <- inference_opts[["samples"]]
      if (all(grepl("value", names(tidied_output)))) {
        samples <- tidied_output
      } else {
        samples <- get_samples(tidied_output, num_samples)
      }
      samples$Iteration <- 1:num_samples
      ggmcmc_samples <- tidyr::gather_(
        samples, key_col = "Parameter", value_col = "value",
        gather_cols = names(samples)[names(samples) != "Iteration"],
        factor_key = TRUE
      )
      ggmcmc_samples$Chain <- chain

      attr(ggmcmc_samples, "nChains") <- chains
      attr(ggmcmc_samples, "nParameters") <- ncol(samples) - 1
      attr(ggmcmc_samples, "nIterations") <- inference_opts[["samples"]]
      attr(ggmcmc_samples, "nBurnin") <- inference_opts[["burn"]]
      attr(ggmcmc_samples, "nThin") <- inference_opts[["thin"]]
      attr(ggmcmc_samples, "description") <- ""
      ggmcmc_samples
    } else if (output_format=="samples" & !is.null(inference_opts)) {
      num_samples <- inference_opts[["samples"]]
      if (all(grepl("value", names(tidied_output)))) {
        samples <- tidied_output
      } else {
        samples <- get_samples(tidied_output, num_samples)
      }
      samples
    } else {
      tidied_output
    }
  } else {
    model_output
  }
}

#' webppl
#'
#' Runs a webppl program.
#'
#' @param program_code A string of a webppl program.
#' @param program_file A file containing a webppl program.
#' @param data A data frame (or other serializable object) that can be
#'   referenced in the program.
#' @param data_var A name by which data can be referenced in the program.
#' @param packages A character vector of external package names to use.
#' @param model_var The name by which the model be referenced in the program.
#' @param inference_opts Options for inference
#' (see http://webppl.readthedocs.io/en/master/inference.html)
#' @param output_format An optional string indicating posterior output format:
#' "webppl" probability table (default), "samples" for just the samples,
#' "ggmcmc" for use with ggmcmc package.
#' @param chains Number of chains (this run is one chain).
#' @param chain Chain number of this run.
run_webppl <- function(program_code = NULL, program_file = NULL, data = NULL,
                       data_var = NULL, packages = NULL, model_var = NULL,
                       inference_opts = NULL, output_format = "webppl", chains = NULL,
                       chain = 1) {

  # find location of rwebppl JS script, within rwebppl R package
  script_path <- file.path(rwebppl_path(), "js/rwebppl")
  add_packages <- packages

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
      add_packages <- c(add_packages, file.path(tmp_dir, data_var))
    }
  }

  # set modified_program_code to program_code or to contents of program_file
  if (!is.null(program_code)) {
    if (!is.null(program_file)) {
      warning("both program_code and program_file supplied, using program_code")
    }
    modified_program_code <- program_code
  } else if (!is.null(program_file)) {
    if (!file.exists(program_file)) {
      stop("program_file does not exist")
    }
    modified_program_code <- paste(readLines(program_file, warn = FALSE),
                                   collapse = "\n")
  } else {
    stop("supply one of program_code or program_file")
  }

  # if inference_opts and model_var supplied, add an Infer call to the program
  if (!is.null(inference_opts)) {
    if (is.null(model_var)) {
      stop("when supplying inference_opts, you must also supply model_var")
    }
    infer <- sprintf("Infer(JSON.parse('%s'), %s)",
                     jsonlite::toJSON(inference_opts, auto_unbox = TRUE),
                     model_var)
    modified_program_code <- paste(modified_program_code, infer, sep = "\n")
  }

  # write modified_program_code to temporary file and store its name in file_arg
  cat(modified_program_code, file = (file_arg <- tempfile()))

  # set output_arg to path to temporary file with a unique key
  uid <- uuid::UUIDgenerate()
  output_arg <- sprintf("/tmp/webppl_output_%s", uid)

  # create --require argument out of each package name
  if (!is.null(add_packages)) {
    package_args <- unlist(lapply(add_packages,
                                  function(x) paste("--require", x)))
  } else {
    package_args <- ""
  }

  # clear paths where rwebppl JS script will write output, errors, or finish file
  output_file <- output_arg
  if (file.exists(output_file)) {
    file.remove(output_file)
  }
  error_file <- "/tmp/webppl_error"
  if (file.exists(error_file)) {
    file.remove(error_file)
  }
  finish_file <- sprintf("/tmp/webppl_finished_%s", uid)
  finish_arg <- finish_file
  if (file.exists(finish_file)) {
    file.remove(finish_file)
  }

  # run rwebppl JS script with model file and packages as arguments
  # any output to stdout gets sent to the R console while command runs
  system2(script_path, args = c(file_arg, output_arg, finish_arg, package_args),
          stdout = "", stderr = "", wait = FALSE)

  # wait for output file or error file to exist
  while (!(file.exists(finish_file))) {
    Sys.sleep(0.25)
  }

  # if the command produced output, collect and tidy the results
  if (file.exists(output_file)) {
    output_string <- paste(readLines(output_file, warn = F),
                           collapse = "\n")
    if (output_string != "") {
      output <- jsonlite::fromJSON(output_string, flatten = TRUE)
      tidy_output(output, output_format = output_format, chains = chains,
                  chain = chain, inference_opts = inference_opts)
    }
  }
}

# declare i as a global variable to avoid NOTE from foreach using NSE
globalVariables("i")

#' webppl
#'
#' Runs a webppl program.
#'
#' @importFrom foreach "%dopar%"
#' @inheritParams run_webppl
#' @param chains Number of times to run the program (defaults to 1).
#' @param cores Number of cores to use when running multiple chains (defaults to
#'   1).
#'
#' @return The program's return value(s).
#' @export
#'
#' @examples
#' program_code <- "flip(0.5)"
#' webppl(program_code)
webppl <- function(program_code = NULL, program_file = NULL, data = NULL,
                   data_var = NULL, packages = NULL, model_var = NULL,
                   inference_opts = NULL, chains = 1, cores = 1,
                   output_format = "webppl") {

  run_fun <- function(k) run_webppl(program_code = program_code,
                                    program_file = program_file,
                                    data = data,
                                    data_var = data_var,
                                    packages = packages,
                                    model_var = model_var,
                                    inference_opts = inference_opts,
                                    output_format = output_format,
                                    chains = chains,
                                    chain = k)
  if (chains == 1) {
    run_fun(1)
  } else {
    doParallel::registerDoParallel(cores = cores)
    chain_outputs <- foreach::foreach(i = 1:chains) %dopar% run_fun(i)
    if (output_format!="webppl") {
      Reduce(rbind, chain_outputs)
    } else {
      chain_outputs
    }
  }
}
