#' Synchronize software category keywords with the IMCR vocabulary
#'
#'
#' @return
#'   (list) Updated software JSON as a list object.
#'   (.json file) Updated software JSON written to the IMCR Portal, and to
#'   \code{paste0(tempdir(), "/", software, ".json")}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' login()
#' sync_software_category()
#' }
#'
sync_software_category <- function(software){
  
  # Check for session string
  if (!exists("imcr_session_string")) {
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
        "Create it with 'get_json()."
      )
    )
  }
  
  # Get software categories
  json <- imcr_json[names(imcr_json) == software]
  cats <- json[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']]
  
  
  # Add/remove user specified keywords
  if (action == 'add') {
    
    newcats <- data.frame(
      rep('EnumerationEntity', length(keywords)),
      rep('', length(keywords)),
      rep('', length(keywords)),
      rep('http://ontosoft.org/software#SoftwareCategory', length(keywords)),
      keywords,
      keywords,
      stringsAsFactors = FALSE
    )
    names(newcats) <- c("@type", "id", "name", "type", "label", "value")
    cats <- rbind(cats, newcats)
    json[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- cats
    
  } else if (action == 'remove') {
    
    newcats <- cats
    newcats <- newcats[newcats$label != keywords]
    if (all(dim(newcats) == c(2,0))) {
      json[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- list()
    } else {
      json[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- newcats
    }
    
  }
  
  # Add/remove broader keywords
  
  # Write to file
  jsonlite::write_json(
    json[[1]],
    path = paste0(tempdir(), "/", software, ".json"),
    auto_unbox = TRUE
  )
  
  # Upload
  put_software(
    software, 
    imcr_session_string, 
    paste0(tempdir(), "/", software, ".json")
  )
  
  return(json[[1]])
  
}
