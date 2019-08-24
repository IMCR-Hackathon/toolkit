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
#'   (character) Session string.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Enter credentials to function
#' login('myname', 'mypass')
#' # Have credentials requested in the console
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
  sstr <- jsonlite::fromJSON(
    httr::content(r, as = 'text', encoding = 'UTF-8')
  )$sessionString

  return(sstr)

}
