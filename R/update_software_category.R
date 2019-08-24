#' Update software category
#'
#' Add, remove, and align software category keywords.
#'
#' @param software
#'   (character) Software name
#' @param keywords
#'   (character) New keywords
#' @param method
#'   (character) "add" (add new keyword), "remove" (remove existing keyword),
#'   or "align" (align with a controlled vocabulary.
#' @param json
#'   (list) JSON of all OntoSoft Portal software. Create this list with
#'   \code{get_all_software()}.
#'
#' @return
#'   (json) Updated JSON object and a JSON file written to \code{tempdir()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Add new keyword and all broader terms
#' update_software_category(
#'   'arrow',
#'   c('workflows', 'loading'),
#'   'add',
#'   json
#' )
#' }
#'
update_software_category <- function(software, json){
  r <- json[names(json) == software]
  return(r)
}
