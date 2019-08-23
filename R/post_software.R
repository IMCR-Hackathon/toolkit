#' Add new software
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
#' post_software('arrow', sstr, '/path/to/software')
#' }
#'
post_software <- function(software.name, session.string, path){

  # Add software
  r <- httr::POST(
    url = 'http://imcr.ontosoft.org/repository/software',
    body = upload_file(paste0(path, '/', software.name, '.json')),
    add_headers(`X-Ontosoft-Session` = session.string)
  )

  # Return status code
  message('Status code: ', paste0(r$status_code))

}
