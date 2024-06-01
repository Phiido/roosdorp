#' Helper function for easier printing of Rmd files
#'
#' `print_rmd()`returns labels from a data dictionary, either renaming the columns or
#'  returns a character vector with the labels. The latter may be useful when wanting
#'  to label variables used in plots or tables.
#'
#' @param name Name of Rmd file that will be used. Locates in './Rmd/'.
#' @param params Pass along parameters/data to the Rmd
#' @param output_path Defaults './output'; Where to output rendered Rmd files
#' @param open Logical; Open the rendered file when done?
#' @param quiet Logical; Should rendering be quiet?
#'
#' @return NULL
#'
#' @export
print_rmd <- function(name, params = NULL, output_path = './output', open = FALSE, quiet = TRUE) {

  base::stopifnot(base::is.character(name), base::is.character(output_path))

  rmd_file <- base::paste0('./Rmd/', name, '.Rmd')
  output_file <- base::file.path(output_path, type, '.pdf')

  base::stopifnot(base::file.exists(rmd_file))

  rmarkdown::render(intput = rmd_file,
                    params = params,
                    output_dir = output_path,
                    envir = new.env(),
                    quiet = TRUE)

  # quick command
  if (open) {
    invisible(system(paste('open', output_file)))
  }
}
