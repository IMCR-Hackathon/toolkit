#' Synchronize software with source metadata
#' 
#' Uses two URLs listed in the intial, manually entered, OntoSoft metadata 
#' record. These are: (1.) The official archive in which the software is 
#' published (e.g. 
#' CRAN (https://cran.r-project.org/web/packages/codemetar/index.html), 
#' PyPI (https://pypi.org/project/certbot-dns-route53/)) listed under the 
#' OntoSoft object property \code{hasProjectWebsite} (i.e. in Portal Speak
#' \code{"Identify" > "Locate" > "Is there a project website for the software?"}), 
#' and (2.) The official location of where the software is being 
#' developmed/maintained (e.g. GitHub (https://github.com/ropensci/antiword)) 
#' listed under the OntoSoft object property \code{hasActiveDevelopment} (i.e. 
#' in Portal Speak \code{"Update" > "Contribute" > "How is the software being 
#' developed or maintained?"}). Software metadata is extracted from these 
#' two sources cross walked through the \code{codemetar} crosswalk  and used 
#' to update the content of the OntoSoft metadata record.
#'
#' @param name
#'   (character) Software name(s). Use \code{name = "all_imcr_software"} 
#'   to update all IMCR software metadata.
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
#' 
#' }
#'
sync_software_metadata <- function(name){
  
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
  
  # Loop through each software
  for (i in 1:length(name)) {
    
    # Get software json from OntoSoft and URL of software source code
    json <- imcr_json[names(imcr_json) == name[i]][[1]]
    url <- json$value[['http://ontosoft.org/software#hasCodeLocation']]$value
    
    if (!is.null(url)) {
      # Get codemeta for source archive and development repository
      if (isTRUE(stringr::str_detect(url, "cran"))) {
        pkg <- stringr::str_extract(url, "(?<=/)[:alnum:]*(?=_)")
        cm <- codemetar::create_codemeta_cran(pkg)
        if (length(cm$codeRepository) > 0) {
          url_dev <- cm$codeRepository
          if (isTRUE(stringr::str_detect(url_dev, "github"))) {
            gh_owner <- stringr::str_extract(
              url_dev, 
              "(?<=github.com/)[:graph:]*(?=/[:graph:]*$)"
            )
            gh_repo <- stringr::str_extract(
              url_dev, 
              paste0(
                "(?<=", 
                stringr::str_extract(
                  url_dev, 
                  "(?<=github.com/)[:graph:]*(?=/[:graph:]*$)"
                ), 
                "/).*"
              )
            ) 
            cm_dev <- jsonlite::fromJSON(
              codemetar::crosswalk(
                gh::gh(
                  "/repos/:owner/:repo", 
                  owner = gh_owner, 
                  repo = gh_repo
                ), 
                "GitHub"
              )
            ) 
          }
          # Combine codemeta sources (cm and cm_dev)
          
        }
      }
    }
    
    # Crosswalk codemeta into OntoSoft

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
      # Send notification
      message(paste0("Software category keywords of '", name[i], "' have been added."))
      # Update the imcr_json and imcr_json_mod_index objects
      imcr_json[names(imcr_json) == name[i]][[1]] <<- json
      imcr_json_mod_index[names(imcr_json) == name[i]] <<- TRUE
      
    } else if (action == 'remove') {
      
      use_i <- cats$label %in% term
      cats <- cats[!use_i, ]
      if (all(dim(cats) == c(2,0))) {
        json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- list()
      } else {
        json$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- cats
      }
      # Send notification
      message(paste0("Software category keywords of '", name[i], "' have been removed."))
      # Update the imcr_json and imcr_json_mod_index objects
      imcr_json[names(imcr_json) == name[i]][[1]] <<- json
      imcr_json_mod_index[names(imcr_json) == name[i]] <<- TRUE
      
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
        # Send notification
        message(paste0("Software category keywords of '", name[i], "' have been replaced."))
        # Update the imcr_json and imcr_json_mod_index objects
        imcr_json[names(imcr_json) == name[i]][[1]] <<- json
        imcr_json_mod_index[names(imcr_json) == name[i]] <<- TRUE
      }
      
    }
    
  }

}
