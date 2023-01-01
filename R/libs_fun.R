# 'install_libs' based on stackoverflow user 4125693; 2017-06-20

#' Install from a list of packages
#'
#' `install_libs()` checks for existing packages and ask for permission to install if any are missing.
#'
#' @param lib What group of packages is wanted. Default is "all".
#'
#' @details
#' "base" - Main packages used by this package
#'
#' "epi" - Packages for epidemiological data handling
#'
#' "mdown" - Packages for different type of markdown handling
#'
#' "bench" - Packages for benchmarking purposes
#'
#' "dev" - Packages for development purposes
#'
#' @return NULL
#'
#' @export
install_libs <- function(lib = "all") {

  lib_names <- extract_libs(lib)

  if (purrr::is_empty(lib_names)) {
    stop("Could not find valid grouping of packages.")
  }

  # Find the indices of the packages that are already installed
  installed_indices <- which(lib_names %in% installed.packages())
  # Extract the names of the packages that need to be installed
  install_lib_names <- lib_names[-installed_indices]
  # Count the number of packages to install
  num_libs <- length(install_lib_names)

  if (num_libs == 0)
    return(message("All packages already installed!"))

  # Create a message to prompt the user to install the missing libraries
  if (num_libs > 2) {
    libs_msg <-
      paste0(paste0(install_lib_names[1:(num_libs - 1)], collapse = ", "), ",")
  } else {
    libs_msg <- install_lib_names[1]
  }

  libs_msg <-
    paste(
      "The following packages could not be found: ",
      libs_msg,
      "\n\r\n\rInstall missing packages?",
      collapse = ""
    )

  # Prompt the user to install the missing libraries
  if (utils::winDialog(type = c("yesno"), libs_msg) == "YES") {
    utils::install.packages(install_lib_names)
  }

  invisible(NULL)
}

#' Uninstalls packages from a pre-determined set
#' @param lib Default NULL; Must explicitly state which set of packages.
#'
#' See `install_libs()` for further information.
#'
#' @export
remove_libs <- function(lib) {

  is_lib_null(lib)
  lib_names <- extract_libs(lib)

  # Find the indices of the packages that are installed
  installed_indices <- which(lib_names %in% installed.packages())
  # Extract the names of the installed packages
  installed_libs <- lib_names[installed_indices]
  # Count the number of packages to be removed
  num_libs <- length(installed_libs)

  for (package in installed_libs) {
    utils::remove.packages(package)
  }

  # Print a message indicating the number of libraries removed
  message(paste("Removed", num_libs, "libraries."))

  invisible(NULL)
}

#' Load packages from a pre-determined set.
#'
#' @param lib Default NULL; See `install_libs()` for which sets are available.
#'
#' @export
load_libs <- function(lib) {
  handle_libs(lib, method = "load")
}

#' Unloads packages from a pre-determined set.
#'
#' @param lib Default NULL; See `install_libs()` for which sets are available.
#'
#' @export
unload_libs <- function(lib) {
  handle_libs(lib, method = "unload")
}

#--------------- Helper functions

#' Function to handle input and either load or unload packages
#'
#' @param lib List of package names
#' @param method Whether to load or unload packages
handle_libs <- function(lib, method) {

  # Check that the input is not null
  is_lib_null(lib)

  lib_names <- extract_libs(lib)

  for(package in lib_names) {
    if (method == "load") {
      require(package, character.only = TRUE)
    }
    else if (method == "unload") {
      detach(package, character.only = TRUE)
    }
  }
}

#' Function to extract package names from a pre-determined set of data
#'
#' @param lib List of package names
extract_libs <- function(lib) {
  # Extract the names of the libraries to remove
  if (any(lib == "all")) {
    lib_names <- unlist(packages)
  } else {
    lib_names <- unlist(packages[[lib]])
  }
}

#' Function that terminates parent function if lib is NULL
#'
#' @param lib List of package names
is_lib_null <- function(lib) {
  if (is.null(lib)) {
    stop("'lib' must not be null")
  }
}
