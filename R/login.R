#' Login
#'
#' @usage
#'   login(user.name = NULL, user.pass = NULL)
#'
#' @param user.name
#'   (character) OntoSoft user name. Default (NULL) queries for this
#'   information in the console.
#' @param user.pass
#'   (character) OntoSoft user password. Default (NULL) queries for this
#'   information in the console.
#'
#' @return
#'   (character) Session string object named "imcr_session_string" and created
#'   in the global environment for use with other \code{toolkit} functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' }
#'
login <- function(user.name = NULL, user.pass = NULL){

  # Query for credentials
  if (any(is.null(user.name), is.null(user.pass))){
    user.name <- readline('Enter user name: ')
    user.pass <- readline('Enter user password: ')
  }

  # Make request
  r <- httr::POST(
    url = 'http://imcr.ontosoft.org/repository/login',
    body = list(name = user.name, password = user.pass),
    encode = 'json'
  )

  # Get session string
  imcr_session_sting <<- jsonlite::fromJSON(
    httr::content(r, as = 'text', encoding = 'UTF-8')
  )$sessionString

  # Notify user
  message("You are now logged in.")
  

}
