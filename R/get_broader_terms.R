#' Get broad terms of an IMCR Vocabulary keyword
#'
#' @param term
#'   (character) IMCR Vocabulary keyword
#'
#' @return
#'   (list) Named list of terms (including searched term) and corresponding
#'   IDs. NA is returned for non-existant terms.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_broad_terms("workflow planning")
#' }
#'
get_broad_terms <- function(term) {
  
  # Set API endpoint
  rtematres::rtematres.options(
    tematres_service_url = "http://vocab.lternet.edu/vocab/registry/services.php"
  )
  
  # Continue if term exists
  code <- suppressWarnings(rtematres::rtematres.api.conversion.term_id(term))
  if (!identical(code, logical(0))) {
    
    # Search term, get ID, then get broader terms.
    r <- rtematres::rtematres.api(task = "search", argument = term)
    terms <- rtematres::rtematres.api(
      task = "fetchUp", 
      argument = as.numeric(r$id[r$term == term])
    )
    
    # Add term if missing from list of terms
    if (!isTRUE(term %in% terms$term)) {
      terms$id <- c(terms$id, code)
      terms$term <- c(terms$term, term)
    }
    
    # Remove NAs
    terms$id <- terms$id[!is.na(terms$id)]
    terms$term <- terms$term[!is.na(terms$term)]
    
  } else {
    
    # Return NAs if term doesn't exist
    r <- rtematres::rtematres.api(task = "search", argument = term)
    terms <- rtematres::rtematres.api(
      task = "fetchUp", 
      argument = as.numeric(r$id[r$term == term])
    )
    
  }

  return(terms)
  
}