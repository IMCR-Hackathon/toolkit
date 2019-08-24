#' Get JSON metadata for all portal software
#'
#' @param url
#'   (character) OntoSoft portal URL.
#'
#' @return
#'   (list) Named list of JSON metadata for all software in a specified 
#'   OntoSoft Portal. This object is named "json" and is returned to the global
#'   environment for use with other toolkit functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_json("http://imcr.ontosoft.org/repository/software")
#' }
#'
get_json <- function(url = "http://imcr.ontosoft.org/repository/software"){
  ids <- jsonlite::fromJSON(url)$id
  json <- lapply(ids, jsonlite::fromJSON)
  names(json) <- unlist(
    lapply(seq_along(json), function(x){json[[x]][['label']]}))
  json <<- json
}
