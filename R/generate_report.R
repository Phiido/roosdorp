#'
#' Render reports
#'
#' Render multiple reports from Rmd-template. Can use parallel computing if possible.
#' Make sure there is sufficient memory and CPU available. Rmarkdown/tinytex may throw errors otherwise.
#'
#' @author Thomas Roosdorp
#'
#' @param df Input as data frame
#' @param var Name of variable containing UID for reports
#' @param template File path with Rmd-template for rendering
#'
#' Optional arguments
#' @param by Which variable should be used for grouping outputs, or none
#' @param overwrite Whether existing files should be overwritten
#' @param is_quiet Whether Rmarkdown should be quiet
#' @param output_folder Folder to use for output. Default: './output/reports'
#'
#' @export

generate_report <- function(df, var, template, ...) {
  # TODO Implement pipe function

  args_default <- list(by = NULL,
                       overwrite = TRUE,
                       isQuiet = TRUE,
                       output_folder = file.path(".", "output", "reports"),
                       prefix = NULL) # TODO Add prefix/suffix to output filename

  args_in <- lapply(substitute(list(...)), function(x) ifelse(is.character(x) | is.null(x) | is.logical(x), x, deparse(x)))
  args <- modifyList(args_default, args_in)

  rm(list = c("args_default", "args_in"))

  by <- args$by
  useGroups <- !is_empty(by)
  output_folder <- args$output_folder

  if (is.character(df) && df == "test") {
    df <- as_tibble(mtcars, rownames = "car")

    UIDs <- pull(df, car)
    var = "car"
    template = "./test/report_test.Rmd"
  }

  if (!is.data.frame(df)) stop("Input must be a data frame")
  if (!is.character(template) || !file.exists(template)) stop("Could not find valid template file")
  if (!(is.character(var) && length(var) == 1)) stop("UID needs to be single element char")

  message("\nAttempting to process data and generate reports.. This may take a while")

  # Removes rows with missing values and all duplicates
  df_clean <- df %>%
    filter(!is.na(!!var)) %>%
    unique()

  UIDs <- pull(df_clean,!!var)
  nDiff <- nrow(df) - nrow(df_clean)
  df <- df_clean

  rm(df_clean)

  if (nDiff > 0) warning(message(cat("Removed", nDiff, "duplicates or missing values")))

  # TODO Reverse folder creation if function ends prematurely
  # Create all by group folders
  if (useGroups) {

    by_groups <- df %>%
      select(!!by) %>%
      filter(!is.na(!!by)) %>%
      distinct() %>%
      pull(!!by) %>%
      textclean::replace_non_ascii()

    for (group in by_groups) {
      group_folder <- file.path(output_folder, group)

      if (!dir.exists(group_folder))
        dir.create(group_folder)
    }
  }

  # Check for existing files to exclude if overwrite is FALSE
  if (!args$overwrite) {
    rmUID <- NULL

    if (!useGroups) {
      files <- list.files(output_folder)
      rmUID <- path_ext_remove(files)

    } else { # Else iterate by group

      for (group in by_groups) {
        target_folder <- file.path(output_folder, group)
        files <- list.files(target_folder)
        UID_exist <- path_ext_remove(files)

        append(rmUID, UID_exist)
      }
    }

    nDiff <- length(rmUID)
    UIDs <- setdiff(UIDs, rmUID)

    if (is_empty(UIDs))
      stop("Files already exists and argument overwrite is FALSE")
  }

  nUID <- length(UIDs)
  nCores <- detectCores()
  useParallel <-  nUID > 10 && nCores > 3 # If above certain threshold and enough cores, use parallel computing

  if (nCores < 3)
    message("Not enough cores for parallel computing. Using sequential computing.")

  # Progress combine function
  updateProgress <- function(iterator){
    progressBar <- timerProgressBar(min = 1, max = iterator - 1, style = 3)
    count <- 0

    function(...) {
      count <<- count + length(list(...)) - 1
      setTimerProgressBar(progressBar, count)
      flush.console()
      cbind(...)
    }
  }

  if (useParallel) {

    message("Currently parallel computing does not show a progressbar. Just be patient")
    # Using less cores than max remedies issues with latex; Outfile enables printing
    cluster <- makeCluster(nCores - 2, outfile = "", methods = FALSE)
    registerDoParallel(cluster)

  } else registerDoSEQ() # Else use sequential computing which treats %dopar% as %do%

  # TODO Fix progressbar updating during paralell computing

  # Stops clusters regardless if function finishes
  on.exit({
    if (useParallel) {
      try({
        stopImplicitCluster()
        stopCluster(cluster)
      })
    }
  })

foreach(i = icount(nUID), .combine = updateProgress(nUID), .multicombine = TRUE, .packages = c("dplyr"), .verbose = FALSE) %dopar% {

    file_out <- rmarkdown::render(template,
                                  params = list(df = df,
                                                id = UIDs[i]),
                                  output_file = UIDs[i],
                                  quiet = TRUE)

    if (useGroups) {
      group <- df %>%
        filter((!!as.symbol(var)) == UIDs[i]) %>%
        pull(by)

      target_folder <- file.path(output_folder, group)

    } else target_folder <- output_folder

    # Need to do this way as render() argument of output_dir causes issues with tinytex
    file_name <- basename(file_out)

    file.rename(file_out, file.path(target_folder, file_name))
  }

  # closepb(progress_bar)
  # close(pb)

  if (!args$overwrite && nDiff > 0)
    message(cat("Overwrite is set to FALSE.", "Skipped", nDiff, "reports"))
  message(cat("\nGeneration finished with", nUID, "reports."))
}
