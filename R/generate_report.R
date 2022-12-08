#' Render custom reports from data.frame
#'
#' `generate_report()`renders multiple reports from custom Rmd-template, and
#' uses parallel computing if possible for faster processing.
#'
#' Make sure there is sufficient memory and CPU available. Rmarkdown/tinytex may
#' throw errors otherwise. Mostly an issue if running on a laptop.
#'
#' @param data Input as data frame.
#' @param var Name of variable containing UID for reports.
#' @param template File path with Rmd-template for rendering.
#' @param by Variable to group reports into sub-folders. Default is NULL.
#' @param overwrite Whether existing files should be overwritten.
#' @param output_folder Folder to use for output. Default (relative to project): 'output/reports'.
#' @param ... Additional arguments than can be passed down to
#'   rmarkdown:render(). Argument quiet is set to TRUE.
#'
#' @return NULL
#'
#' @export
generate_report <- function(data,
                            var,
                            template,
                            by = NULL,
                            overwrite = TRUE,
                            output_folder = "./output/reports",
                            ...) {

  if (!is.data.frame(data)) stop("Input must be a data frame")
  if (!file.exists(template)) stop("Could not find valid template file")

  message("Attempting to process data and generate reports..")

  by <- rlang::enquo(by)
  var <- rlang::enquo(var)

  use_groups <- !rlang::quo_is_null(by)

  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

  data_clean <- data |>
    dplyr::filter(!is.na(!!var)) |>
    unique()

  UIDs <- dplyr::pull(data_clean, !!var)
  n_UID <- length(UIDs)
  n_diff <- nrow(data) - nrow(data_clean)

  if (n_diff > 0) warning(message(cat("Skipping", n_diff, "rows that are either duplicates or missing values")))

  # Create all sub-folders if by is specified
  if (use_groups) {

    groups <- data_clean |>
      dplyr::pull(!!by) |>
      stats::na.omit() |>
      unique() |>
      textclean::replace_non_ascii()

    for (group in groups) {
      group_folder <- file.path(output_folder, group)

      if (!dir.exists(group_folder)) dir.create(group_folder)
    }
  }

  # Check for existing files to exclude if overwrite is FALSE
  if (!overwrite) {
    rmUID <- NULL

    if (!use_groups) {
      files <- list.files(output_folder)
      rmUID <- fs::path_ext_remove(files)

    } else { # Else iterate by group folders

      for (group in groups) {
        target_folder <- file.path(output_folder, group)
        files <- list.files(target_folder)
        UID_exist <- fs:path_ext_remove(files)

        append(rmUID, UID_exist)
      }
    }

    n_diff <- length(rmUID)
    UIDs <- setdiff(UIDs, rmUID)
    n_UID <- length(UIDs)

    if (n_UID == 0)
      stop("Files already exists and argument overwrite is FALSE")
  }

  # If above certain threshold and enough cores, use parallel computing
  n_cores <- parallel::detectCores()
  use_parallel <-  n_UID > 10 && n_cores > 2

  if (n_cores < 3)
    message("Not enough cores for parallel computing. Using sequential computing.")

  progress_bar <- pbapply::timerProgressBar(min = 1, max = n_UID - 1, style = 3)

  update_progress <- function() {
    count <- 0

    function(...) {
      count <<- count + length(list(...)) - 1
      pbapply::setTimerProgressBar(progress_bar, count)
      flush.console()
      cbind(...)
    }
  }

  if (use_parallel) {

    # Using less cores than max remedies issues with latex
    cluster <- parallel::makeCluster(n_cores - 1, methods = FALSE)
    doSNOW::registerDoSNOW(cluster)

  } else foreach::registerDoSEQ() # Else use sequential computing which treats %dopar% as %do%

  foreach::foreach(i = 1:n_UID, .combine = update_progress(), .packages = c("dplyr")) %dopar% {

    file_out <- rmarkdown::render(template,
                                  params = list(data = data,
                                                id = UIDs[i]),
                                  output_file = UIDs[i],
                                  quiet = TRUE)

    if (use_groups) {
      group <- data_clean |>
        dplyr::filter((!!var) == UIDs[i]) |>
        dplyr::pull(!!by)

      target_folder <- file.path(output_folder, group)

    } else target_folder <- output_folder

    # Solution for issues with argument output_dir in render() together with tinytex
    file_name <- basename(file_out)

    # Move file to target location
    file.rename(file_out, file.path(target_folder, file_name))

  }

  # Stops clusters regardless if function finishes
  on.exit({
    if (use_parallel) {
      try({
        parallel::stopCluster(cluster)
      })
    }
  })

  pbapply::closepb(progress_bar)

  if (!overwrite && n_diff > 0)
    message(cat("\nOverwrite is set to FALSE.", "Skipped", n_diff, "reports"))

  message(cat("\nGeneration finished with", n_UID, "reports."))
  message(cat("Saved reports at:", output_folder))

  invisible(NULL)
}

# ------------------ Helper Functions -----------------------

# TODO Set and use the same template across a session.
# If empty, return current template if set
set_template <- function() {}

#' Get the example template that is used for render testing
#'
#' @return NULL
#'
#' @export
get_example <- function() {

  template <- system.file("extdata", "report_test.Rmd", package = "roosdorp")
  folder <- "./Rmd"

  if(!dir.exists(folder)) dir.create(folder)

  file_path <- file.path(folder, basename(template))
  file.copy(template, file_path)
  message("Template copied to './Rmd/'")

  invisible(NULL)
}

#' Generate a test report
#'
#' @param ... Optional arguments for parent function
#'
#' @return NULL
#'
#' @export
generate_test <- function(...) {

  data <- dplyr::as_tibble(mtcars, rownames = "car")
  template <- system.file("extdata", "report_test.Rmd", package = "roosdorp")

  generate_report(data, var = car, template, ...)

  invisible(NULL)
}
