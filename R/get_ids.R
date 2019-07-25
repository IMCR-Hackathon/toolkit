#' Get all software IDs of an OntoSoft Portal.
#'
#' @param url
#'   (character) OntoSoft portal URL
#'
#' @return
#'   (list) Named list of software IDs. Names are softare titles.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ids <- get_ids('http://imcr.ontosoft.org/repository/software')
#' }
#'
get_ids <- function(url){
  ids <- jsonlite::fromJSON(url)$id
  return(ids)
}
