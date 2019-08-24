#' Add/remove software category keywords
#'
#' Add/remove only the narrowest terms, broader terms are automatically 
#' added/removed.
#'
#' @param action
#'   (character) Action to perform ("add" or "remove")
#' @param software
#'   (character) Software name
#' @param keywords
#'   (character) Keywords from the 
#'   \href{http://vocab.lternet.edu/vocab/registry/index.php}{IMCR Controlled Vocabulary}.
#' @param session.string
#'   (character) Key generated with \code{login()}.
#'
#' @return
#'   (list) Updated software JSON represented as a list.
#'   (.json file) Updated software JSON written to 
#'   \code{paste0(tempdir(), "/", software, ".json")}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all software JSON from IMCR Portal
#' json <- get_json("http://imcr.ontosoft.org/repository/software")
#' 
#' # Get session string
#' sstr <- login()
#' 
#' # Add software category keywords
#' update_software_category("add", 'arrow', c('quality control', 'import'), sstr)
#' 
#' # Remove software category keywords
#' update_software_category("remove", 'arrow', c('quality control', 'import'), sstr)
#' }
#'
update_software_category <- function(action, software, keywords, session.string){
  
  # Check for json
  if (!exists("json") | !is.list(json)){
    stop(
      paste0(
        "The object 'json' is missing from the global environment.",
        "Create it with 'get_json()."
      )
    )
  }
  
  # Get software categories
  metadata <- json[names(json) == software]
  categories <- metadata[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']]
  

  # Add/remove software categories
  if (action == 'add'){
    
    new_categories <- data.frame(
      rep('EnumerationEntity', length(keywords)),
      rep('', length(keywords)),
      rep('', length(keywords)),
      rep('http://ontosoft.org/software#SoftwareCategory', length(keywords)),
      keywords,
      keywords,
      stringsAsFactors = FALSE
    )
    names(new_categories) <- c("@type", "id", "name", "type", "label", "value")
    categories <- rbind(categories, new_categories)
    metadata[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- categories
    
  } else if (action == 'remove'){
    
    new_categories <- categories
    new_categories <- new_categories[new_categories$label != keywords]
    if (all(dim(new_categories) == c(2,0))){
      metadata[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- list()
    } else {
      metadata[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- new_categories
    }
    
  }
  
  # Write to file
  jsonlite::write_json(
    metadata[[1]],
    path = paste0(tempdir(), "/", software, ".json"),
    auto_unbox = TRUE
  )

  # Upload
  put_software(
    software, 
    session.string, 
    paste0(tempdir(), "/", software, ".json")
  )

  # Return
  return(metadata[[1]])
  
}
