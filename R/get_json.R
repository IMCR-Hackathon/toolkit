#' Get JSON metadata for all portal software
#'
#' @param url
#'   (character) OntoSoft portal URL.
#'
#' @return
#'   (list) Named list of software JSON metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' json <- get_json("http://imcr.ontosoft.org/repository/software")
#' }
#'
get_json <- function(url = "http://imcr.ontosoft.org/repository/software"){
  ids <- jsonlite::fromJSON(url)$id
  json <- lapply(ids, jsonlite::fromJSON)
  names(json) <- unlist(
    lapply(seq_along(json), function(x){json[[x]][['label']]}))
  return(json)
}
