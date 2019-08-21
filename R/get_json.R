#' Get JSON metadata for all portal software
#'
#' @param ids
#'   (character) OntoSoft software identifiers.
#'
#' @return
#'   (list) JSON metadata for each identifier.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ids <- get_ids('http://imcr.ontosoft.org/repository/software')
#' json <- get_json(ids)
#' }
#'
get_json <- function(ids){
  json <- lapply(ids, jsonlite::fromJSON)
  return(json)
}
