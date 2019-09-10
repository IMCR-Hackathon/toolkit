#' Add/remove/replace software category keywords
#'
#' @param action
#'   (character) Action to perform: "add", "remove", or "replace".
#' @param name
#'   (character) Software name(s). Use \code{name = "all_imcr_software"} 
#'   to apply the \code{action} to all IMCR software.
#' @param term
#'   (character) Keyword(s) to "add" or "remove". If adding, use terms from the
#'   \href{http://vocab.lternet.edu/vocab/registry/index.php}{IMCR Controlled Vocabulary}.
#' @param old.term
#'   (character) Keyword to be replaced (only used with 
#'   \code{action = "replace"}).
#' @param new.term
#'   (character) Keyword to replace \code{old.term} (only used with 
#'   \code{action = "replace"}).
#'
#' @return
#'   (list) Updated software JSON for the specified \code{name} and added to 
#'   the \code{imcr_json} object in the global environment.
#'   (logical) Updated \code{imcr_json_mod_index} object in the global 
#'   environment, which indicates the specified \code{name} has been modified
#'   and is used by \code{put_software()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all IMCR software JSON metadata
#' get_imcr_json()
#' 
#' # Add terms
#' modify_software_category("add", "arrow", c("quality control", "import"))
#' 
#' # Remove terms
#' modify_software_category("remove", "arrow", "quality control")
#' 
#' # Replace terms
#' modify_software_category("replace", "arrow", old.term = "import", new.term = "loading")
#' 
#' # Login to theIMCR and update modified software metadata
#' login()
#' put_software()
#' }
#'
modify_software_category <- function(action, name, term, old.term = NULL, new.term = NULL){
  
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
  
  # Unify "term" and "new.term", as they mean the same thing.
  if (!is.null(new.term)) {
    term <- new.term
  }
  
  # Get names of all IMCR software (if necessary)
  if (all(name == "all_imcr_software")) {
    name <- names(imcr_json)
  }
  
  # Exclude any terms not within the IMCR Vocabulary and send warning.
  term <- unlist(
    lapply(
      seq_along(term), 
      function(x){
        if (any(is.na(get_broad_terms(term[x])$term))) {
          message(
            paste0(
              "'", 
              term[x],
              "'",
              " is not apart of the IMCR Vocabulary and will not be added."
            )
          )
        } else {
          return(term[x])
        }
      }
    )
  )
  
  # Continue if valid terms have been entered
  if (!is.null(term)) {
    
    for (i in 1:length(name)) {
      
      # Get software json and category keywords
      json <- imcr_json[names(imcr_json) == name[i]][[1]]
      cats <- json$value[['http://ontosoft.org/software#hasSoftwareCategory']]
      
      # Add/remove user specified keywords
      if (action == 'add') {
        
        newcats <- data.frame(
          rep('EnumerationEntity', length(term)),
          rep('', length(term)),
          rep('', length(term)),
          rep('http://ontosoft.org/software#SoftwareCategory', length(term)),
          term,
          term,
          stringsAsFactors = FALSE
        )
        names(newcats) <- c("@type", "id", "name", "type", "label", "value")
        cats <- rbind(cats, newcats)
        json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- cats
        
      } else if (action == 'remove') {
        
        use_i <- cats$label %in% term
        cats <- cats[!use_i, ]
        if (all(dim(cats) == c(2,0))) {
          json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- list()
        } else {
          json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- cats
        }
        
      } else if (action == "replace") {
        
        use_i <- cats$label %in% old.term
        if (any(use_i)) {
          cats <- cats[!use_i, ]
          newcats <- data.frame(
            rep('EnumerationEntity', length(term)),
            rep('', length(term)),
            rep('', length(term)),
            rep('http://ontosoft.org/software#SoftwareCategory', length(term)),
            term,
            term,
            stringsAsFactors = FALSE
          )
          names(newcats) <- c("@type", "id", "name", "type", "label", "value")
          cats <- rbind(cats, newcats)
          if (all(dim(cats) == c(2,0))) {
            json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- list()
          } else {
            json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- cats
          }
        }

      }
      
      # Update the imcr_json and imcr_json_mod_index objects
      imcr_json[names(imcr_json) == name[i]][[1]] <<- json
      imcr_json_mod_index[names(imcr_json) == name[i]] <<- TRUE
      
      # Send notification
      message(paste0("Software category keywords of '", name[i], "' have been updated."))
      
    }

  }

}
