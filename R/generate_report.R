#' Render custom reports from data.frame
#'
#' `generate_report()`renders multiple reports from custom Rmd-template, and
#' uses parallel computing if possible for faster processing.
#'
#' It uses two Rmarkdown parameters that needs to be present in the template in order for proper rendering:
#' df and id. These can then be used in the Rmarkdown file as usual for data handling or visualization.
#' See the example template for proper setup, `get_example()`.
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
#' Rendering markdown files on a network drive will cause issues.
#' As per: https://github.com/rstudio/rmarkdown/issues/1268
#' A solution to this is keeping the project and output folders on a hard drive.
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

  # Convert the group and id variables to symbols
  by <- rlang::enquo(by)
  id_var <- rlang::enquo(var)

  # Check if the group argument was specified
  use_groups <- !rlang::quo_is_null(by)

  # Create the output folder if it does not exist
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

  # Remove rows with missing or duplicate values for the id variable
  data_clean <- data |>
    dplyr::filter(!is.na(!!id_var)) |>
    unique()

  # Extract the unique values of the id variable
  ids <- dplyr::pull(data_clean, !!id_var)
  n_ids <- length(ids)

  # Calculate the number of rows that were removed
  n_diff <- nrow(data) - nrow(data_clean)

  # Print a warning if rows were removed
  if (n_diff > 0) warning(message(paste("Skipping", n_diff, "rows that are either duplicates or missing values")))

  # Create sub-folders for each group if a group variable was specified
  if (use_groups) {

    groups <- data_clean |>
      dplyr::pull(!!by) |>
      stats::na.omit() |>
      unique() |>
      textclean::replace_non_ascii()

    # Create a sub-folder for each group
    for (group in groups) {
      group_folder <- file.path(output_folder, group)

      if (!dir.exists(group_folder)) dir.create(group_folder)
    }
  }

  # Check for existing files if overwrite is FALSE
  if (!overwrite) {

    # Initialize a vector to store the IDs of existing files
    existing_ids <- NULL

    if (!use_groups) {

      # Extract the names of the existing files in the output folder
      files <- list.files(output_folder)
      # Remove the file extensions from the file names
      existing_ids <- fs::path_ext_remove(files)

    } else { # Else iterate by group folders

      # Iterate over each group
      for (group in groups) {
        # Construct the path to the group's output folder
        group_folder <- file.path(output_folder, group)
        # Extract the names of the existing files in the group's output folder
        files <- list.files(group_folder)
        # Remove the file extensions from the file names
        group_ids <- fs::path_ext_remove(files)
        # Append the IDs of the existing files to the list of existing IDs
        existing_ids <- c(existing_ids, group_ids)
      }
    }

    # Calculate the number of IDs that are already associated with existing files
    n_diff <- length(existing_ids)
    # Remove the IDs of the existing files from the list of IDs
    ids <- dplyr::setdiff(ids, existing_ids)
    # Calculate the number of remaining IDs
    n_ids <- length(ids)

    # Check if all IDs are already associated with existing files
    if (n_ids == 0)
      stop("Files already exists and argument overwrite is FALSE")
  }

  # If above certain threshold and enough cores, use parallel computing
  n_cores <- parallel::detectCores()
  use_parallel <-  n_ids > 10 && n_cores > 2

  # Check if the number of cores is sufficient for parallel computing
  if (n_cores < 3)
    message("Not enough cores for parallel computing. Using sequential computing.")

  # Initialize a progress bar to track the progress of the loop
  progress_bar <- pbapply::timerProgressBar(min = 1, max = n_ids - 1, style = 3)

  # Define a function to update the progress bar when used in a foreach loop
  update_progress <- function() {
    count <- 0

    function(...) {
      count <<- count + length(list(...)) - 1
      pbapply::setTimerProgressBar(progress_bar, count)
      flush.console()
      cbind(...)
    }
  }

  # Check if parallel computing should be used
  if (use_parallel) {

    # Create a cluster with one fewer cores than the maximum number of cores
    # This remedies issues with LaTeX
    cluster <- parallel::makeCluster(n_cores - 1, methods = FALSE)
    doSNOW::registerDoSNOW(cluster)

    # Else use sequential computing, which treats %dopar% as %do%
  } else foreach::registerDoSEQ()

  # Iterate over each ID in the list
  foreach::foreach(i = 1:n_ids, .combine = update_progress(), .packages = c("dplyr")) %dopar% {

    # Render the template with the specified data and ID
    file_out <- rmarkdown::render(template,
                                  params = list(data = data,
                                                id = ids[i]),
                                  output_file = ids[i],
                                  quiet = TRUE)

    # Determine the target folder for the rendered file
    if (use_groups) {
      group <- data_clean |>
        dplyr::filter((!!id_var) == UIDs[i]) |>
        dplyr::pull(!!by)

      target_folder <- file.path(output_folder, group)

    } else target_folder <- output_folder

    # Solution for issues with argument output_dir in render() together with tinytex
    file_name <- basename(file_out)

    # Move the rendered file to the target folder
    file.rename(file_out, file.path(target_folder, file_name))

  }

  # Stop the cluster when the function finishes, regardless of success
  on.exit({
    if (use_parallel) {
      try({
        parallel::stopCluster(cluster)
      })
    }
  })

  # Close the progress bar
  pbapply::closepb(progress_bar)

  # Print a message indicating the number of reports that were generated and skipped
  if (!overwrite && n_diff > 0)
    message(cat("\nOverwrite is set to FALSE.", "Skipped", n_diff, "reports"))

  message(cat("\nGeneration finished with", n_ids, "reports."))
  message(cat("Saved reports at:", output_folder))

  # Return an invisible NULL value
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

  # Specify the path to the template file
  template_file <- system.file("extdata", "report_test.Rmd", package = "roosdorp")

  # Specify the output folder
  output_folder <- "./Rmd"

  # Check if the output folder exists
  if(!dir.exists(output_folder)) dir.create(output_folder)

  # Construct the full path for the copied template file
  copied_template_path <- file.path(output_folder, basename(template_file))
  # Copy the template file to the output folder
  file.copy(template_file, copied_template_path)

  # Print a message indicating that the template was copied
  message("Template copied to './Rmd/'")

  # Return an invisible NULL value
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
  # Convert the mtcars data frame to a tibble
  data <- dplyr::as_tibble(mtcars, rownames = "car")
  # Specify the path to the template file
  template_file <- system.file("extdata", "report_test.Rmd", package = "roosdorp")

  # Call the generate_report function to generate the reports
  generate_report(data, var = car, template_file, ...)

  # Return an invisible NULL value
  invisible(NULL)
}
