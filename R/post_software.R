#' Add new software
#'
#' @param software.name
#'   (character) Software name
#' @param path
#'   (list) Path to software JSON.
#'
#' @return
#'   (message) Status message of operation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sstr <- login()
#' post_software('arrow', sstr, '/path/to/software')
#' }
#'
post_software <- function(software.name, path){

  # Check for session string
  if (!exists("imcr_session_string")){
    stop(
      paste0(
        "The object 'imcr_session_string' is missing from the global environment.",
        "Create it with 'login()."
      )
    )
  }
  
  # Add software
  r <- httr::POST(
    url = 'http://imcr.ontosoft.org/repository/software',
    body = upload_file(paste0(path, '/', software.name, '.json')),
    add_headers(`X-Ontosoft-Session` = imcr_session_string)
  )

  # Return status code
  message('Status code: ', paste0(r$status_code))

}
