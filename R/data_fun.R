#' Renames variable names based on data dictionary
#'
#' `label_vars()`returns labels from a data dictionary, either renaming the columns or
#'  returns a character vector with the labels. The latter may be useful when wanting
#'  to label variables used in plots or tables.
#'
#' @param data Input as data frame or character.vector
#' @param dict Input as data.frame with two columns. Should closely follow a key value structure
#' @param simple Logical; Should return as character.vector? Otherwise renamed data.frame
#'
#' @return Data.frame or character.vector
#'
#' @export
label_vars <- function(data,
                       dict = NULL,
                       simple = FALSE) {

  base::stopifnot(base::is.data.frame(data) | base::is.character(data),
                  base::is.data.frame(dict),
                  base::is.logical(simple))

  if (is.character(data) & length(data) !=2) stop('Dictionary must have length of 2')

  # TODO add functionality by reading rownames

  if (base::is.data.frame(data)) {
    names_vars <- base::names(data)
  } else {
    names_vars <- data
  }

  # for readability
  names(dict) <- c('key', 'value')

  vars <- NULL

  for (name in names_vars) {
    if (name %in% dict[,'key']) {
      vars <- append(vars, dict[dict[, 'key'] == name, 'value'])
    } else vars <- append(vars, name)
  }

  if (is.data.frame(data)) names(data) <- vars

  if (simple & is.data.frame(data)) return(names(data))
  else return(data)
}
