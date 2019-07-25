#' Get JSON metadata for all portal software
#'
#' @param url
#'   (character) Portal URL
#'
#' @return
#'   (list) JSON metadata for all portal software
#'
#' @export
#'
#' @examples
#' \dontrun{
#' json <- get_json('http://imcr.ontosoft.org/repository/software')
#' }
#'
get_json <- function(url){
  ids <- jsonlite::fromJSON(url)$id
  json <- lapply(ids, jsonlite::fromJSON)
  return(json)
}
