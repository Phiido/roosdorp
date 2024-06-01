#' Renames variable names based on data dictionary
#'
#' `label_vars()`returns labels from a data dictionary, either renaming the columns or
#'  returns a character vector with the labels. The latter may be useful when wanting
#'  to label variables used in plots or tables.
#'
#' @param data Input as data frame or character.vector
#' @param dict Either data.frame or file.path to dictionary
#' @param simple Logical; Should return as character.vector? Otherwise renamed data.frame
#'
#' @return Data.frame or Character vector
#'
#' @export
label_vars <- function(data,
                       dict = NULL,
                       simple = FALSE) {

  base::stopifnot(base::is.data.frame(data), base::is.character(data))

  dictionary <- get('dictionary', envir = .ProjectEnv)

  group <- switch(dict,
                  data      = dictionary$data,
                  food      = dictionary$food,
                  foodgroup = dictionary$foodgroup,
                  model     = dictionary$model,
                  vars      = dictionary$vars)

  group <- group[c('id','description')] |>
    as.data.frame()

  names(data) <- group[match(names(data), group$id), 'description']

  invisible(data)
}
