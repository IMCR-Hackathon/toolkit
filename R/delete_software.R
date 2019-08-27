#' Delete software from the IMCR Portal
#'
#' @param name
#'   (character) Name of software
#'
#' @return
#'   (message) Status message of DELETE operation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_imcr_json()
#' login()
#' delete_software("arrow")
#' }
#'
delete_software <- function(name){

  # Check for imcr_json object
  if (!exists("imcr_json") | !is.list(imcr_json)) {
    stop(
      paste0(
        "The object 'imcr_json' is missing from the global environment.",
        "Create it with 'get_json()."
      )
    )
  }
  
  # Check for imcr_session_string object
  if (!exists("imcr_session_string")){
    stop(
      paste0(
        "The object 'imcr_session_string' is missing from the global environment.",
        "Create it with 'login()."
      )
    )
  }
  
  # Get software JSON
  if (!any(names(imcr_json) == name)){
    stop(paste0(name, " doesn't exist."))
  }
  metadata <- imcr_json[names(imcr_json) == name]

  # Delete software
  r <- httr::DELETE(
    url = metadata[[1]]$id,
    httr::add_headers(`X-Ontosoft-Session` = imcr_session_string))

  # Return status code
  message(paste0('Status code: ', r$status_code))

}
