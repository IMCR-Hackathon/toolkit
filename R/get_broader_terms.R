#' Get broader terms for a given IMCR Vocabulary keyword
#'
#' @param keyword
#'   (character) IMCR Vocabulary keyword
#'
#' @return
#'   (character) The input term and any broader terms related to it. A warning
#'   is issued if the term was not found.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_broader_terms("workflow planning")
#' }
#'
get_broader_terms <- function(term){
  
  # # Check for session string
  # if (!exists("imcr_session_string")){
  #   stop(
  #     paste0(
  #       "The object 'imcr_session_string' is missing from the global environment.",
  #       "Create it with 'login()."
  #     )
  #   )
  # }
  
  # Warn if term doesn't exist
  
  # Set API endpoint
  rtematres.options(tematres_service_url = "http://vocab.lternet.edu/vocab/registry/services.php")
  
  # Search term and get ID
  term <- "workflow planning"
  r <- rtematres.api(task = "search", argument = term)
  
  # fetchUp ID to get broader terms. Top terms return "NA" when fetching up.
  broadterms <- rtematres.api(task = "fetchUp", argument = as.numeric(r$id[r$term == term]))
  
  # # Get software JSON
  # metadata <- jsonlite::read_json(paste0(path, '/', software.name, '.json'))
  # 
  # # Update software
  # r <- httr::PUT(
  #   url = metadata[[1]]$id,
  #   body = upload_file(paste0(path, '/', software.name, '.json')),
  #   add_headers(`X-Ontosoft-Session` = imcr_session_string)
  # )
  # 
  # # Return status code
  # message('Status code: ', paste0(r$status_code))
  
}