#' Get JSON of all software in the IMCR Portal
#'
#' @return
#'   (list) List of JSON metadata for all software in the IMCR Portal. This 
#'   object is named "imcr_json" and is returned to the global environment 
#'   where it is accessed by other \code{toolkit} functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_imcr_json()
#' }
#'
get_imcr_json <- function(){
  ids <- jsonlite::fromJSON("http://imcr.ontosoft.org/repository/software")$id
  imcr_json <- lapply(ids, jsonlite::fromJSON)
  names(imcr_json) <- unlist(
    lapply(seq_along(imcr_json), function(x){imcr_json[[x]][['label']]}))
  imcr_json <<- imcr_json
}