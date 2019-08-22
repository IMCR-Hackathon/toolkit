#' Update software category
#'
#' Update all software category keywords to the current structure of the IMCR
#' Controlled Vocabulary.
#'
#' @param software
#'   (character) Software name
#' @param keywords
#'   (character) New keywords
#' @param action
#'   (character) "add" or "remove" software category
#' @param json
#'   (list) JSON of all OntoSoft Portal software. Create this list with
#'   \code{get_json()}.
#'
#' @return
#'   (json) Updated JSON object and a JSON file written to \code{tempdir()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' update_software_category()
#' }
#'
update_SoftwareCategory <- function(software, json){
  r <- json[names(json) == software]
  return(r)
}
