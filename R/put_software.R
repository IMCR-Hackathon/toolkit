#' Update software in the IMCR Portal
#'
#' Modified software metadata (indicated by a "TRUE" in the 
#' \code{imcr_json_mod_index} object) is written to \code{tempdir()} and 
#' uploaded to the IMCR Portal.
#'
#' @return
#'   (message) Status message of PUT operation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' get_imcr_json()
#' modify_software_category("add", "arrow", c("quality control", "import"))
#' put_software()
#' }
#'
put_software <- function(){

  # Check for imcr_session_string object
  if (!exists("imcr_session_string")){
    stop(
      paste0(
        "The object 'imcr_session_string' is missing from the global environment.",
        "Create it with 'login()."
      )
    )
  }
  
  # Check for imcr_json object
  if (!exists("imcr_json") | !is.list(imcr_json)) {
    stop(
      paste0(
        "The object 'imcr_json' is missing from the global environment.",
        "Create it with 'get_imcr_json()."
      )
    )
  }
  
  # Check for imcr_json_mod_index object
  if (!exists("imcr_json_mod_index") | !is.logical(imcr_json_mod_index)) {
    stop(
      paste0(
        "The object 'imcr_json_mod_index' is missing from the global environment.",
        "Create it with 'get_imcr_json()."
      )
    )
  }
  
  # Check if software has been modified
  if (!any(imcr_json_mod_index)) {
    stop("No software has been modified and no attempt at updating will be made.")
  }
  
  # Update IMCR Portal software
  for (i in seq(length(imcr_json_mod_index))[imcr_json_mod_index]) {
    
    # Write software to file
    jsonlite::write_json(
      imcr_json[[i]],
      path = paste0(tempdir(), "/", imcr_json[[i]]$label, ".json"),
      auto_unbox = TRUE,
      null = "null"
    )
    
    # Update software
    r <- httr::PUT(
      url = imcr_json[[i]]$id,
      body = httr::upload_file(paste0(tempdir(), '/', imcr_json[[i]]$label, '.json')),
      httr::add_headers(`X-Ontosoft-Session` = imcr_session_string)
    )
    
    # Return status code
    message(
      paste0(
        "Updating IMCR Software ",
        imcr_json[[i]]$label,
        " Status code: ", 
        r$status_code
      )
    )

  }

}
