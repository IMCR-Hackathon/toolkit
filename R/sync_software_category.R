#' Synchronize software category keywords with the IMCR vocabulary
#'
#' @param name
#'   (character) Software name
#'
#' @return
#'   (list) Updated software JSON for the specified \code{name} and added to 
#'   the \code{imcr_json} object in the global environment.
#'   (logical) Updated \code{imcr_json_mod_index} object in the global 
#'   environment, which indicates the specified \code{name} has been modified
#'   and is used by PUT and POST functions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_imcr_json()
#' modify_software_category("add", "arrow", "thesauri")
#' sync_software_category("arrow")
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
  
  # Get software json and category keywords
  json <- imcr_json[names(imcr_json) == name][[1]]
  cats <- json$value[['http://ontosoft.org/software#hasSoftwareCategory']]
  
  # Get broad terms, validate all terms, and notify user of changes.
  r <- unlist(
    lapply(
      seq_along(cats$label),
      function(x){
        get_broad_terms(cats[x, "label"])$term
      }
    )
  )
  r <- r[!is.na(r)]
  terms_added <- r[!(r %in% cats$label)]
  terms_removed <- cats$label[!(cats$label %in% r)]
  message(paste0("Updating software category keywords for '", name, "'."))
  message(paste0("Terms added: ", paste0(terms_added, collapse = ", ")))
  message(paste0("Terms removed: ", paste0(terms_removed, collapse = ", ")))
  
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
  imcr_json[names(imcr_json) == name][[1]] <<- json
  imcr_json_mod_index[names(imcr_json) == name] <<- TRUE
  
  # Send notification
  message(
    paste0(
      "Software category keywords of '", 
      name, 
      "' have been synchronized with the IMCR Vocabulary."
    )
  )

}
