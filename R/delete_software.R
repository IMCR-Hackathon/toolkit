#' Delete software from portal
#'
#' @param software.name
#'   (character) Software name
#' @param session.string
#'   (character) Key generated with \code{login()}.
#' @param json
#'   (list) JSON of all portal software. Create this list with
#'   \code{get_json()}.
#'
#' @return
#'   (message) Status message of operation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sstr <- login()
#' delete_software('arrow', sstr)
#' }
#'
delete_software <- function(software.name, session.string, json){

  # Get software metadata
  if (!any(names(json) == software.name)){
    stop(paste0(software.name, " doesn't exist."))
  }
  metadata <- json[names(json) == software.name]

  # Delete software
  r <- httr::DELETE(
    url = metadata[[1]]$id,
    add_headers(`X-Ontosoft-Session` = session.string))

  # Return status code
  message('Status code: ', paste0(r$status_code))

}
