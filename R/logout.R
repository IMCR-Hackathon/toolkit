#' Logout from an IMCR session
#'
#' @usage
#'   logout()
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' logout()
#' }
#'
logout <- function(){
  
  # # Check for imcr_session_string object
  if (!exists("imcr_session_string")){
    stop(
      paste0(
        "The object 'imcr_session_string' is missing from the global environment. ",
        "No users are logged into IMCR at this time."
      )
    )
  }

  # # Place request
  r <- httr::POST(
    url = "http://imcr.ontosoft.org/repository/logout",
    httr::add_headers(
      `X-Ontosoft-Session` = imcr_session_string, 
      `Content-Type` = "application/json",
      `X-HTTP-Method-Override` = "POST"
    ),
    body = jsonlite::toJSON(
      x = list(
        sessionid = stringr::str_extract(imcr_session_string, "(?<=\\|).*$")
      ), 
      auto_unbox = TRUE
    )
  )
  
  # Remove session string
  rm(imcr_session_string, pos = 1)
  
  # Notify user
  message("You are now logged out.")
  
}