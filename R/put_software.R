#' Update software
#'
#' @param software.name
#'   (character) Software name
#' @param session.string
#'   (character) Key generated with \code{login()}.
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
#' put_software('arrow', sstr, '/path/to/software')
#' }
#'
put_software <- function(software.name, session.string, path){

  # Update software
  r <- httr::DELETE(
    url = metadata[[1]]$id,
    add_headers(`X-Ontosoft-Session` = session.string))

  # Return status code
  message('Status code: ', paste0(r$status_code))

}
