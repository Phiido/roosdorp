# 'install_libs' based on stackoverflow user 4125693; 2017-06-20

#' Install from a list of packages
#'
#' `install_libs()` checks for packages (user input or pre-determined packages;
#'   see below) and ask for permission to install if any are missing.
#'
#' @param lib Default 'all'; Vector of either package names or use internal sets
#' @param internal Default 'TRUE'; Whether to use internal pre-determined sets
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
install_libs <- function(lib = "all", internal = TRUE) {

  stopifnot('lib must be character vector' = is.character(lib))

  if (internal) {
    lib_names <- extract_libs(lib)
  } else lib_names <- lib

  if (purrr::is_empty(lib_names)) {
    stop("Could not find valid grouping of packages.")
  }

  installed_indices <- which(lib_names %in% installed.packages())
  install_lib_names <- lib_names[-installed_indices]
  n_libs <- length(install_lib_names)

  if (n_libs == 0) {
    return(message("All packages already installed!"))
  }

  # Create a message to prompt the user to install the missing libraries
  if (n_libs > 2) {
    libs_msg <- paste0(paste0(install_lib_names[1:(n_libs - 1)], collapse = ", "), ",")
  } else {
    libs_msg <- install_lib_names[1]
  }

  libs_msg <- paste("The following packages could not be found: ",
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
  n_libs <- length(installed_libs)

  for (package in installed_libs) {
    utils::remove.packages(package)
  }

  message(paste("Removed", n_libs, "libraries."))

  invisible(lib)
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

#' Retrieves and prints what the packages are in the pre-determined sets as a character vector
#'
#' @param group Default 'all'; Group name of pre-determined sets of packages
#'
#' @export
print_libs <- function(group = 'all') {
  return(print(extract_libs(group)))
}

# Helper functions ------------------------------------------------------------

#' Function to handle input and either load or unload packages
#'
#' @param lib List of package names
#' @param method Whether to load or unload packages
#'
#' @noRd
handle_libs <- function(lib, method) {
  is_lib_null(lib)

  lib_names <- extract_libs(lib)

  for (package in lib_names) {
    if (method == "load") {
      require(package, character.only = TRUE)
    } else if (method == "unload") {
      detach(package, character.only = TRUE)
    }
  }
}

#' Function to extract package names from a pre-determined set of data
#'
#' @param lib List of package names
#'
#' @noRd
extract_libs <- function(lib) {
  if (any(lib == "all")) {
    lib_names <- unname(unlist(packages))
  } else {
    lib_names <- unname(unlist(packages[[lib]]))
  }
}

#' Function that terminates parent function if lib is NULL
#'
#' @param lib List of package names
#'
#' @noRd
is_lib_null <- function(lib) {
  if (is.null(lib)) {
    stop("'lib' must not be null")
  }
}
