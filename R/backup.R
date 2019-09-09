#' Back up all software metadata in the IMCR Portal
#'
#' @param path
#'   (character) Where IMCR metadata will be written.
#'
#' @return
#'   (.json) JSON metadata files written to the specified \code{path}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_imcr_json()
#' backup("/path/to/backup/directory")
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
  
  # Create directory
  dir.create(paste0(path, "/", format(Sys.Date(), "%Y%m%d"), "_imcr_backup"))
  
  # Write to file
  lapply(
    seq_along(imcr_json),
    function(x) {
      message("Writing ", names(imcr_json[x]), ".json")
      jsonlite::write_json(
        imcr_json[[x]],
        path = paste0(
          path, 
          "/", 
          format(Sys.Date(), "%Y%m%d"), 
          "_imcr_backup/", 
          names(imcr_json[x]), ".json"
        ),
        auto_unbox = TRUE
      )
    }
  )
  
  
  # Return status code
  message('Done.')
  
}
