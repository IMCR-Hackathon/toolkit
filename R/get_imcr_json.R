#' Get JSON of all software in the IMCR Portal
#'
#' @return
#'   (list) List of JSON metadata for all software in the IMCR Portal. This 
#'   object is named "imcr_json" and is returned to the global environment 
#'   where it is accessed by other \code{toolkit} functions.
#'   (list) Logical index indicating whether any of the "imcr_json" objects 
#'   have been modified. This object is named "imcr_json_mod_index" and is 
#'   returned to the global environment where it is accessed by other 
#'   \code{toolkit} functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_imcr_json()
#' }
#'
get_imcr_json <- function(){
  
  # Don't overwrite imcr_json object if it exists
  if (exists("imcr_json")) {
    stop("The object 'imcr_json' exists and will not be overwritten.")
  }
  
  # Get json and create the json modification index
  message("Getting IMCR JSON.")
  ids <- jsonlite::fromJSON("http://imcr.ontosoft.org/repository/software")$id
  imcr_json <- lapply(
    ids, 
    function(x) {
      json <- try(jsonlite::fromJSON(x), silent = TRUE)
      if (class(json) == 'try-error') {
        NA_character_
      } else {
        json
      }
    }
  )
  is_unreadable <- is.na(imcr_json)
  imcr_json <- imcr_json[!is_unreadable]
  names(imcr_json) <- unlist(
    lapply(seq_along(imcr_json), function(x) {imcr_json[[x]][['label']]})
  )
  imcr_json <<- imcr_json
  imcr_json_mod_index <<- rep(FALSE, length(imcr_json))
  message("Done.")
  
}
