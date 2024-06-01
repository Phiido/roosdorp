#' Export the R project folder as a compressed file
#'
#' `backup_project()`exports the R project folder for easy backup and/or sharing local versions
#'
#' It exports files present in the working directory of the current R project.
#' It is flexible in terms of output location and white- and blacklisting of files to include.
#' Due note, by default it does not export any files that are present in ./data/ in case
#' of any sensitive data present. However, this is available to bypass as an argument.
#'
#' @param overwrite Overwrite target file or not. Is bypassed if timestamp is TRUE.
#' @param prefix What the exported project file should use as prefix. Defaults to 'project name' using basename()
#' @param timestamp If TRUE, adds a timestamp in addition to a date-suffix.
#' @param output_path Defaults to ./backup/
#' @param include_data Should subdirectory of ./data/ be included in export?
#' @param ignore Additional files to ignore (Currently not implemented!)
#' @param include Additional files to include (Currently not implemented!)
#' @param ... Additional arguments that can be passed down
#'
#' @return NULL
#'
#' @export
backup_project <- function(overwrite    = FALSE,
                           prefix       = NULL,
                           timestamp    = FALSE,
                           output_path  = NULL,
                           include_data = FALSE,
                           ignore       = NULL,
                           include      = NULL,
                           ...) {


  base::stopifnot(base::is.logical(overwrite),
                  base::is.logical(timestamp),
                  base::is.logical(include_data))

  base::stopifnot(base::is.null(prefix) | base::is.character(prefix),
                  base::is.null(output_path) | base::is.character(output_path),
                  base::is.null(ignore) | base::is.character(ignore),
                  base::is.null(include) | base::is.character(include))

  args <- list('overwrite'   = overwrite,
               'prefix'      = prefix,
               'timestamp'   = timestamp,
               'output_path' = output_path,
               'data'        = include_data,
               'ignore'      = ignore,
               'include'     = include,
               ...)

  if (base::is.null(args$prefix)) {
    args$prefix <- base::paste0(basename(getwd()), '_')
  } else args$prefix <- base::paste0(args$prefix, '_')

  if (base::is.null(args$output_path)) args$output_path <- './backup/'
  if (!('format' %in% base::names(args))) args$format <- 'zip'

  stopifnot(args$format %in% c('zip', '7zip', 'rar'))
  if(all(args$overwrite, args$timestamp)) warning(message = "Ignored 'overwrite' as 'timestamp' is TRUE")

  ignore_git  <- c('.git', '.gitattributes', '.gitignore')
  ignore_dev  <- c('.Rbuildignore')
  ignore_r    <- c('.', '..', '.Rproj.user', 'renv', '.RData', '.Rhistory')

  ignore_files <- c(ignore_git, ignore_dev, ignore_r, 'backup')

  # whether to include or exclude data folder (searches in './data/')
  if (!include_data) {
    ignore_files <- base::append(ignore_files, c('data'))
  }

  # TODO implement black- and whitelisting
  # # i.e. blacklisting files from user input
  # if (!base::is.null(args$ignore)) {
  #   ignore_files <- base::append(ignore_files, args$ignore)
  # }
  #
  # # i.e. whitelisting files from user input
  # if (!base::is.null(args$include)) {
  #   include_files <- append(include_files, args$include)
  # }

  all_files <- base::list.files(all.files = TRUE)
  include_files <- all_files[!(all_files %in% ignore_files)]

  files <- include_files[fs::is_file(include_files)]
  dirs <- base::setdiff(include_files, files)

  if (!base::dir.exists(args$output_path)) {

    msg <- paste('Could not find directory at', args$output_path,
                 '\n\r\n\rWould you like to create it?',
                 collapse = '')

    if (utils::winDialog(type = 'yesno', message = msg) == 'YES') base::dir.create(args$output_path)
    else stop('Missing directory')
  }

  timestamp <- if (args$timestamp) base::format(base::Sys.time(), '%Y-%m-%d_%H-%M-%S') else base::Sys.Date()

  zip_ext <- if (args$format == '7zip') '7z' else args$format
  zip_name <- base::paste0(args$prefix, timestamp, '.', zip_ext)
  zip_loc <- base::file.path(args$output_path, zip_name)
  zip_exists <- fs::file_exists(zip_loc)

  if (zip_exists & !args$overwrite) {
    base::simpleError('File already exists and overwrite is FALSE')
  }

  tmp_folder <- 'temp'

  # remove temp folder and files when function ends
  on.exit({
    if (dir.exists(tmp_folder)) unlink(tmp_folder, recursive = TRUE, force = TRUE)
  })

  if (!dir.exists(tmp_folder)) dir.create(tmp_folder)

  for (file in files) {
    path_to <- fs::path(tmp_folder, file)
    fs::file_copy(file, path_to, overwrite = TRUE)
  }

  for (dir in dirs) {
    path_to <- fs::path(tmp_folder, dir)
    fs::dir_copy(dir, path_to, overwrite = TRUE)
  }

  # renv folder handling is special
  if ('renv' %in% dirs) {
    renv_origin <- 'renv/activate.R'
    tmp_renv <- fs::path(tmp_folder, base::dirname(renv_origin))

    if(!fs::dir_exists(tmp_renv)) fs::dir_create(tmp_renv)

    fs::file_copy(renv_origin, fs::path(tmp_folder, renv_origin))
  }

  # TODO find workaround
  # .Rprofile is even more special due to limitations in the archive package not seeing hidden files
  if ('.Rprofile' %in% files) {
    base::file.rename('temp/.Rprofile', 'temp/_Rprofile')
  }

  is_tmp_folder_empty <- base::length(base::list.files(path = tmp_folder, all.files = TRUE, include.dirs = TRUE)) == 0

  if (!is_tmp_folder_empty) {
    archive::archive_write_dir(archive = zip_name, dir = tmp_folder, format = args$format)
    fs::file_move(zip_name, zip_loc)
  } else base::simpleError(message = 'Could not find any files to export!')

  if (fs::file_exists(zip_loc)) message(paste('Successfully exported project as', args$format, 'to', args$output_path))
  else base::simpleError(message = "Something went wrong when exporting project..")

  base::invisible()
}
