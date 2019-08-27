#' Add new software to the IMCR Portal
#'
#' @param name
#'   (character) File name of software JSON (without file extension).
#' @param path
#'   (character) Path to software JSON.
#'
#' @return
#'   (message) Status message of POST operation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' post_software("arrow", "/path/to/software/json")
#' }
#'
post_software <- function(name, path){

  # Check for imcr_session_string object
  if (!exists("imcr_session_string")){
    stop(
      paste0(
        "The object 'imcr_session_string' is missing from the global environment.",
        "Create it with 'login()."
      )
    )
  }
  
  # Add software JSON
  if (!file.exists(paste0(path, '/', name, '.json'))){
    stop(paste0(name, " doesn't exist."))
  }
  r <- httr::POST(
    url = 'http://imcr.ontosoft.org/repository/software',
    body = httr::upload_file(paste0(path, '/', name, '.json')),
    httr::add_headers(`X-Ontosoft-Session` = imcr_session_string)
  )

  # Return status code
  message(paste0('Status code: ', r$status_code))

}
