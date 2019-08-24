#' Add/remove software category keywords
#'
#' Only the narrowest terms need to 
#' be added, \code{align_software_category()} automatically adds and removes
#' broader terms of a specified vocabulary.
#'
#' @param action
#'   (character) "add" or "remove" software category keywords
#' @param software
#'   (character) Software name
#' @param keywords
#'   (character) Software category keywords
#' @param session.string
#'   (character) Key generated with \code{login()}.
#'
#' @return
#'   (json file) Software JSON file written to 
#'   \code{paste0(tempdir(), '/', software)}.
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
#' set_software_category("add", 'arrow', c('quality control', 'import'), sstr)
#' 
#' # Remove software category keywords
#' set_software_category("remove", 'arrow', c('quality control', 'import'), sstr)
#' }
#'
set_software_category <- function(action, software, keywords, session.string){
  
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
