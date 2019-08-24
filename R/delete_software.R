#' Delete software from portal
#'
#' @param software.name
#'   (character) Software name
#'
#' @return
#'   (message) Status message of operation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' delete_software('arrow')
#' }
#'
delete_software <- function(software.name){

  # Check for session string
  if (!exists("imcr_session_string")){
    stop(
      paste0(
        "The object 'imcr_session_string' is missing from the global environment.",
        "Create it with 'login()."
      )
    )
  }
  
  # Get software metadata
  if (!any(names(imcr_json) == software.name)){
    stop(paste0(software.name, " doesn't exist."))
  }
  metadata <- imcr_json[names(imcr_json) == software.name]

  # Delete software
  r <- httr::DELETE(
    url = metadata[[1]]$id,
    add_headers(`X-Ontosoft-Session` = imcr_session_string))

  # Return status code
  message('Status code: ', paste0(r$status_code))

}
