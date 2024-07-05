#' Helper function for easier printing of Rmd files
#'
#' `print_rmd()` can be seen as an extension of rmarkdown::render(). You can specify parameters,
#' output folder and whether to open it up after finishing the render.
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

  # TODO handle other file types in a more flexible way
  output_file <- base::file.path(output_path, '.pdf')

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
