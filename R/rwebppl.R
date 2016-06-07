

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

get_samples <- function(df, num_samples) {
  rows <- rep.int(seq_len(nrow(df)), times = round(df$prob * num_samples))
  cols <- names(df) != "prob"
  df[rows, cols, drop = FALSE]
}

tidy_output <- function(model_output, ggmcmc = FALSE, chains = NULL,
                        chain = NULL, inference_opts = NULL) {
  if (!is.null(names(model_output)) &&
      length(names(model_output)) == 2 &&
      all(names(model_output) %in% c("probs", "support"))) {
    if (class(model_output$support) == "data.frame") {
      support <- model_output$support
    } else {
      support <- data.frame(support = model_output$support)
    }
    tidied_output <- cbind(support, data.frame(prob = model_output$probs))
    if (ggmcmc & !is.null(inference_opts) & !is.null(chain) & !is.null(chains)) {
      num_samples <- inference_opts[["samples"]]
      samples <- get_samples(tidied_output, num_samples)
      samples$Iteration <- 1:num_samples
      ggmcmc_samples <- tidyr::gather_(samples, "Parameter", "value",
                                       names(samples)[names(samples) != "Iteration"])
      ggmcmc_samples$Chain <- chain

      attr(ggmcmc_samples, "nChains") <- chains
      attr(ggmcmc_samples, "nParameters") <- ncol(samples) - 1
      attr(ggmcmc_samples, "nIterations") <- inference_opts[["samples"]]
      attr(ggmcmc_samples, "nBurnin") <- inference_opts[["burn"]]
      attr(ggmcmc_samples, "nThin") <- inference_opts[["thin"]]
      attr(ggmcmc_samples, "description") <- ""
      ggmcmc_samples
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
#' @param model_code A string of a webppl program.
#' @param model_file A file containing a webppl program.
#' @param data A data frame (or other serializable object) that can be
#'   referenced in the program.
#' @param data_var A name by which data can be referenced in the program.
#' @param model_packages A character vector of external package names to use.
#' @param model_var The name by which the model be referenced in the program.
#' @param inference_opts Options for inference
#' (see http://webppl.readthedocs.io/en/master/inference.html)
#' @param ggmcmc Logical indicating whether to transform output to ggmcmc format.
#' @param chains Number of chains (this run is one chain).
#' @param chain Chain number of this run.
run_webppl <- function(model_code = NULL, model_file = NULL, data = NULL,
                       data_var = NULL, model_packages = NULL, model_var = NULL,
                       inference_opts = NULL, ggmcmc = FALSE, chains = NULL,
                       chain = 1) {

  # find location of rwebppl JS script, within rwebppl R package
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

  # set modified_model_code to model_code or to contents of mode_file
  if (!is.null(model_code)) {
    if (!is.null(model_file)) {
      warning("both model_code and model_file supplied, using model_code")
    }
    modified_model_code <- model_code
  } else if (!is.null(model_file)) {
    if (!file.exists(model_file)) {
      stop("model_file does not exist")
    }
    modified_model_code <- paste(readLines(model_file), collapse = "\n")
  } else {
    stop("supply one of model_code or model_file")
  }

  # if model_var and inference_opts supplied, add an Infer call to the program
  if (!is.null(model_var) & !is.null(inference_opts)) {
    modified_model_code <- paste(
      modified_model_code,
      sprintf("Infer(JSON.parse('%s'), %s)",
              jsonlite::toJSON(inference_opts, auto_unbox = TRUE),
              model_var),
      collapse = "\n"
    )
  }

  # write modified_model_code to temporary file and store its name in file_arg
  cat(modified_model_code, file = (file_arg <- tempfile()))

  # set output_arg to path to temporary file with a unique key
  output_arg <- sprintf("/tmp/webppl_output_%s", uuid::UUIDgenerate())

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
      output <- jsonlite::fromJSON(output_string, flatten = TRUE)
      tidy_output(output, ggmcmc = ggmcmc, chains = chains, chain = chain,
                  inference_opts = inference_opts)
    }
  }
}

#' webppl
#'
#' Runs webppl model.
#'
#' @importFrom foreach "%dopar%"
#' @inheritParams run_webppl
#' @param chains Number of times to run model (defaults to 1).
#' @param cores Number of cores to use when running multiple chains (defaults to
#'   1).
#'
#' @return The model's return value(s).
#' @export
#'
#' @examples
#' model_code <- "flip(0.5)"
#' webppl(model_code)
webppl <- function(model_code = NULL, model_file = NULL, data = NULL,
                   data_var = NULL, model_packages = NULL, model_var = NULL,
                   inference_opts = NULL, chains = 1, cores = 1,
                   ggmcmc = FALSE) {

  run_fun <- function(k) run_webppl(model_code = model_code,
                                    model_file = model_file,
                                    data = data,
                                    data_var = data_var,
                                    model_packages = model_packages,
                                    model_var = model_var,
                                    inference_opts = inference_opts,
                                    ggmcmc = ggmcmc,
                                    chains = chains,
                                    chain = k)
  if (chains == 1) {
    run_fun(1)
  } else {
    doParallel::registerDoParallel(cores = cores)
    chain_outputs <- foreach::foreach(i = 1:chains) %dopar% run_fun(i)
    if (ggmcmc) {
      Reduce(rbind, chain_outputs)
    } else {
      chain_outputs
    }
  }
}
