# Function based on stackoverflow user 4125693; 2017-06-20

#' Install from a list of packages
#'
#' `get_libs()` checks for existing packages and ask for permission if any are missing.
#'
#' @param lib What group of packages is wanted. Default is "all".
#' base - Main packages used by this package
#' epi - Packages for epidemiological data handling
#' markdown - Packages for different type of markdown handling
#' bench - Packages for benchmarking purposes
#' dev - Packages for development purposes
#'
#' @return NULL
#'
#' @export
get_libs <- function(lib = "all") {

  libs <- list(base = list("tidyverse", "writexl", "remotes", "fs", "rlang",
                           "pbapply", "textclean", "foreach", "doParallel"),
               epi = list("Epi", "MortalityTables"),
               stat = list("infer", "pwr", "naivebayes", "pROC", "rpart",
                           "randomForest", "vtreat", "reclin"),
               mdown = list("rmarkdown", "bookdown", "tinytex", "knitr"),
               bench = list("lobstr", "profvis","benchmarkme", "bench"),
               dev = list("devtools", "roxygen2", "styler",
                          "lintr", "withr", "available", "R6")
               )

  if (lib == "all") libs_vec <- unlist(libs)
  if (lib != "all") libs_vec <- unlist(libs[[lib]])
  if (length(libs_vec) == 0 | is.null(libs_vec))
    stop(" Could not find valid grouping of packages. See Docs.")

  req <- unlist(lapply(libs_vec, require, character.only = TRUE))
  need <- libs_vec[req == FALSE]
  n <- length(need)

  if (n > 0) {
    libsmsg <-
      if (n > 2)
        paste(paste(need[1:(n - 1)], collapse = ", "), ",", sep = "")
    else
      need[1]
    if (n > 1) {
      libsmsg <- paste(libsmsg, " and ", need[n], sep = "")
    }

    libsmsg <-
      paste(
        "The following packages could not be found: ",
        libsmsg,
        "\n\r\n\rInstall missing packages?",
        collapse = "")

    if (utils::winDialog(type = c("yesno"), libsmsg) == "YES") {
      utils::install.packages(need)
      lapply(need, require, character.only = TRUE)
    }
  } else message("\nAll packages installed!")
}
