#' Render custom reports from data.frame
#'
#' `generate_report()`renders multiple reports from custom Rmd-template, and
#'   uses parallel computing if possible for faster processing.
#'
#' It uses two Rmarkdown parameters that needs to be present in the template
#'   in order for proper rendering:
#' df and id. These can then be used in the Rmarkdown file as usual for data
#' handling or visualization.
#' See the example template for proper setup, `get_example()`.
#'
#' Make sure there is sufficient memory and CPU available. Rmarkdown/tinytex may
#' throw errors otherwise. Mostly an issue if running on a laptop.
#'
#' Rendering markdown files on a network drive will cause issues.
#' As per: https://github.com/rstudio/rmarkdown/issues/1268.
#' A solution to this is keeping the project and output folders on a hard drive.
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
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!file.exists(template)) {
    stop("Could not find valid template file")
  }

  message("Attempting to process data and generate reports..")

  # Convert the group and id variables to symbols
  by <- rlang::enquo(by)
  id_var <- rlang::enquo(var)

  # Check if the group argument was specified
  use_groups <- !rlang::quo_is_null(by)

  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

  data_clean <- data |>
    dplyr::filter(!is.na(!!id_var)) |>
    unique()

  ids <- dplyr::pull(data_clean, !!id_var)
  n_ids <- length(ids)

  n_diff <- nrow(data) - nrow(data_clean)

  if (n_diff > 0) {
    warning(message(
      "Skipping",
      n_diff,
      "rows that are either duplicates or missing values"
    ))
  }

  # Create sub-folders for each group if a group variable was specified
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

  # Check for existing files if overwrite is FALSE
  if (!overwrite) {
    existing_ids <- NULL

    if (!use_groups) {
      files <- list.files(output_folder)
      existing_ids <- fs::path_ext_remove(files)
    } else { # Else iterate by group folders

      for (group in groups) {
        group_folder <- file.path(output_folder, group)
        files <- list.files(group_folder)
        group_ids <- fs::path_ext_remove(files)
        existing_ids <- c(existing_ids, group_ids)
      }
    }

    # Calculate the number of IDs that are already associated with existing files
    n_diff <- length(existing_ids)
    # Remove the IDs of the existing files from the list of IDs
    ids <- dplyr::setdiff(ids, existing_ids)
    # Calculate the number of remaining IDs
    n_ids <- length(ids)

    if (n_ids == 0) {
      stop("Files already exists and argument overwrite is FALSE")
    }
  }

  n_cores <- parallel::detectCores()
  use_parallel <- n_ids > 10 && n_cores > 2

  if (n_cores < 3) {
    message("Not enough cores for parallel computing. Using sequential computing.")
  }

  progress_bar <- pbapply::timerProgressBar(min = 1, max = n_ids - 1, style = 3)

  # Helps update the progress bar when used in a foreach loop
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
    cluster <- parallel::makeCluster(n_cores - 1, methods = FALSE)
    doSNOW::registerDoSNOW(cluster)

    # Else use sequential computing, which treats %dopar% as %do%
  } else {
    foreach::registerDoSEQ()
  }

  foreach::foreach(
    i = 1:n_ids,
    .combine = update_progress(),
    .packages = c("dplyr")
  ) %dopar% {
    file_out <- rmarkdown::render(template,
      params = list(
        data = data,
        id = ids[i]
      ),
      output_file = ids[i],
      quiet = TRUE
    )

    # Determine the target folder for the rendered file
    if (use_groups) {
      group <- data_clean |>
        dplyr::filter((!!id_var) == UIDs[i]) |>
        dplyr::pull(!!by)

      target_folder <- file.path(output_folder, group)
    } else {
      target_folder <- output_folder
    }

    # Solution for issues with output_dir in render() together with tinytex
    file_name <- basename(file_out)

    file.rename(file_out, file.path(target_folder, file_name))
  }

  on.exit({
    if (use_parallel) {
      try({
        parallel::stopCluster(cluster)
      })
    }
  })

  pbapply::closepb(progress_bar)

  if (!overwrite && n_diff > 0) {
    message(cat("\nOverwrite is set to FALSE.", "Skipped", n_diff, "reports"))
  }

  message(cat("\nGeneration finished with", n_ids, "reports."))
  message(cat("Saved reports at:", output_folder))

  invisible(data)
}

# Helper Functions ------------------------------------------------------------

# TODO Set and use the same template across a session.
# If empty, return current template if set
set_template <- function() {}

#' Get template that is used for testing
#'
#' @return NULL
#'
#' @export
get_example <- function() {
  template_file <- system.file("extdata", "report_test.Rmd", package = "roosdorp")
  output_folder <- "./Rmd"

  if (!dir.exists(output_folder)) dir.create(output_folder)

  copied_template_path <- file.path(output_folder, basename(template_file))

  file.copy(template_file, copied_template_path)

  message("Template copied to './Rmd/'")
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
  template_file <- system.file("extdata", "report_test.Rmd", package = "roosdorp")

  generate_report(data, var = car, template_file, ...)
}
