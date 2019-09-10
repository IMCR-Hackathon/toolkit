#' Synchronize software category keywords with the IMCR vocabulary
#'
#' @param name
#'   (character) Software name(s). Use \code{name = "all_imcr_software"} 
#'   to synchronize all IMCR software category keywords.
#'
#' @return
#'   (list) Updated software JSON for the specified \code{name} and added to 
#'   the \code{imcr_json} object in the global environment.
#'   (logical) Updated \code{imcr_json_mod_index} object in the global 
#'   environment, which indicates the specified \code{name} has been modified
#'   and is used by \code{put_software()}.
#'   (imcr_log.txt) A log file listing added and removed terms from each 
#'   software.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all IMCR software JSON
#' get_imcr_json()
#' 
#' # Synchronize software category keywords for the "arrow" software.
#' sync_software_category("arrow")
#' 
#' # Synchronize software category keywords for all IMCR software.
#' sync_software_category("all_imcr_software")
#' }
#'
sync_software_category <- function(name){

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
  
  # Get names of all IMCR software (if necessary)
  if (all(name == "all_imcr_software")) {
    name <- names(imcr_json)
  }
  
  # Initialize log file
  path <- paste0(getwd(), "/imcr_log.txt")
  log_con <- file(path)
  cat(
    paste0(Sys.time(), " ==============================================\n"),
    file = log_con, append = TRUE
  )
  
  # Update software category keywords
  for (i in 1:length(name)) {

    # Get software json and category keywords
    json <- imcr_json[names(imcr_json) == name[i]][[1]]
    cats <- json$value[['http://ontosoft.org/software#hasSoftwareCategory']]
    
    # Get broad terms
    r <- unique(
      unlist(
        lapply(
          seq_along(cats$label),
          function(i){
            get_broad_terms(cats[i, "label"])$term
          }
        )
      )
    )
    
    # Continue if terms exist, otherwise there are no terms to update.
    if (!is.null(r)) {
      
      # Validate terms
      r <- r[!is.na(r)]
      terms_added <- r[!(r %in% cats$label)]
      terms_removed <- cats$label[!(cats$label %in% r)]
      message(paste0("Synchronizing software category keywords for '", name[i], "'."))
      message(paste0("Terms added: ", paste0(terms_added, collapse = ", ")))
      message(paste0("Terms removed: ", paste0(terms_removed, collapse = ", ")))
      message("")
      cat(
        paste0("Synchronizing software category keywords for '", name[i], "'.\n"), 
        file = path, 
        append = TRUE
      )
      cat(
        paste0("Terms added: ", paste0(terms_added, collapse = ", "), "\n"), 
        file = path, 
        append = TRUE
      )
      cat(
        paste0("Terms removed: ", paste0(terms_removed, collapse = ", "), "\n\n"), 
        file = path, 
        append = TRUE
      )
      
      # Add valid terms and remove invalid terms
      newcats <- data.frame(
        rep('EnumerationEntity', length(r)),
        rep('', length(r)),
        rep('', length(r)),
        rep('http://ontosoft.org/software#SoftwareCategory', length(r)),
        r,
        r,
        stringsAsFactors = FALSE
      )
      names(newcats) <- c("@type", "id", "name", "type", "label", "value")
      json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- newcats
      
      # Update the imcr_json and imcr_json_mod_index objects
      imcr_json[names(imcr_json) == name[i]][[1]] <<- json
      imcr_json_mod_index[names(imcr_json) == name[i]] <<- TRUE
      
    } else {
      
      # Send notice when keywords don't exist
      message(
        paste0(
          "No software category keywords of '", name[i], 
          "'. Skipping this one.\n"
        )
      )
      cat(
        paste0(
          "No software category keywords of '", name[i], 
          "'. Skipping this one.\n\n"
        ), 
        file = path, 
        append = TRUE
      )
      
    }
    
  }
  
  # Close connection to log file
  close.connection(log_con)

}
