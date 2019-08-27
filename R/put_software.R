#' Update software
#'
#' @param name
#'   (character) File name of software JSON (without file extension).
#' @param path
#'   (character) Path to software JSON.
#'
#' @return
#'   (message) Status message of PUT operation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' put_software("arrow", "/path/to/software/json")
#' }
#'
put_software <- function(name, path){

  # Check for session string
  if (!exists("imcr_session_string")){
    stop(
      paste0(
        "The object 'imcr_session_string' is missing from the global environment.",
        "Create it with 'login()."
      )
    )
  }
  
  # Get software JSON
  metadata <- jsonlite::read_json(paste0(path, '/', name, '.json'))

  # Update software
  r <- httr::PUT(
    url = metadata[[1]]$id,
    body = httr::upload_file(paste0(path, '/', name, '.json')),
    httr::add_headers(`X-Ontosoft-Session` = imcr_session_string)
  )

  # Return status code
  message(paste0('Status code: ', r$status_code))

}
