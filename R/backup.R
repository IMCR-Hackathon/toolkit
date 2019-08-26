#' Back up IMCR JSON to path
#'
#' @param path
#'   (character) Path to where IMCR metadata will be written.
#'
#' @return
#'   (.json) JSON metadata files written to the specified \code{path}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' backup('/Users/csmith/Desktop/imcr_metadata)
#' }
#'
backup <- function(path){
  
  # Check for imcr_json object
  if (!exists("imcr_json") | !is.list(imcr_json)) {
    stop(
      paste0(
        "The object 'imcr_json' is missing from the global environment.",
        "Create it with 'get_json()."
      )
    )
  }
  
  # Write to file
  lapply(
    seq_along(imcr_json),
    function(x) {
      message("Writing ", names(imcr_json[x]), ".json to ", path)
      jsonlite::write_json(
        imcr_json[[x]],
        path = paste0(path, "/", names(imcr_json[x]), ".json"),
        auto_unbox = TRUE
      )
    }
  )
  
  
  # Return status code
  message('Done.')
  
}
