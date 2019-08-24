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

  # Get software JSON
  metadata <- jsonlite::read_json(paste0(path, '/', software.name, '.json'))

  # Update software
  r <- httr::PUT(
    url = metadata[[1]]$id,
    body = upload_file(paste0(path, '/', software.name, '.json')),
    add_headers(`X-Ontosoft-Session` = session.string)
  )

  # Return status code
  message('Status code: ', paste0(r$status_code))

}
