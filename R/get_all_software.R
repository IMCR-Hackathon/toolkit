#' Get all software metadata of an OntoSoft portal
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
#' json <- get_all_software('http://imcr.ontosoft.org/repository/software')
#' }
#'
get_all_software <- function(url){
  json <- get_json(
    jsonlite::fromJSON(url)$id
  )
  return(json)
}
