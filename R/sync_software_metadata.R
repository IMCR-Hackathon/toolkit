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
    
    # Get software json from OntoSoft and the master URL (source archive or 
    # development repository). This assumes the source archive contains
    # information on the development repository.
    json <- imcr_json[names(imcr_json) == name[i]][[1]]
    if (!is.null(json$value[['http://ontosoft.org/software#hasCodeLocation']]$value)) {
      url <- json$value[['http://ontosoft.org/software#hasCodeLocation']]$value
    } else if (is.null(json$value[['http://ontosoft.org/software#hasCodeLocation']]$value) &
               !is.null(json$value[['http://ontosoft.org/software#hasActiveDevelopment']]$value)) {
      url <- json$value[['http://ontosoft.org/software#hasActiveDevelopment']]$value
    }
    
    if (!is.null(url)) {
      
      # Get codemeta for source archive and development repository. 
      # FIXME: Only supported dev repo is GitHub.
      if (isTRUE(stringr::str_detect(url, "cran"))) {
        
        pkg <- stringr::str_extract(url, "(?<=/)[:alnum:]*(?=_)")
        cm_src <- codemetar::create_codemeta_cran(pkg)
        if (length(cm_src$codeRepository) > 0) {
          url_dev <- cm_src$codeRepository
          if (!is.null(url_dev)) {
            if (isTRUE(stringr::str_detect(url_dev, "github"))) {
              cm_dev <- gh_to_codemeta(url_dev)
            }
          }
        }
        
      } else if (isTRUE(stringr::str_detect(url, "github"))) {
        
        cm_src <- gh_to_codemeta(url)
        
      }
      
      # Join codemeta of source archive and development repository. Procede
      # only if codemeta exists, otherwise the OntoSoft software will 
      # experience information loss.
      if (any(c(exists("cm_src"), exists("cm_dev")))) {
        
        # Source and development exist
        if (exists("cm_src") & exists("cm_dev")) {
          
          # CRAN and GitHub
          if (stringr::str_detect(url, "cran") & 
              stringr::str_detect(url_dev, "github")) {
            cm <- join_rpkg_github(cm_src, cm_dev)
          }
          
        # Only development exists
        } else if (exists("cm_src") & !exists("cm_dev")) {
          
          # GitHub
          if (stringr::str_detect(url, "github")) {
            cm <- purrr::compact(cm_src) 
          }
          
        }
        
        # Add codemeta to OntoSoft
        json <- add_codemeta_to_ontosoft(ontosoft = json, cm = cm)
        
        # Update the imcr_json and imcr_json_mod_index objects
        imcr_json[names(imcr_json) == name[i]][[1]] <<- json
        imcr_json_mod_index[names(imcr_json) == name[i]] <<- TRUE
        
      }

    }
    
  }

}




#' Get \code{codemeta} from GitHub URL
#'
#' @param github.url
#'   (character) GitHub URL
#'
#' @return
#'   (codemeta list) A \code{codemeta} list object
#'
gh_to_codemeta <- function(github.url) {
  
  gh_owner <- stringr::str_extract(
    github.url, 
    "(?<=github.com/)[:graph:]*(?=/[:graph:]*$)"
  )
  
  gh_repo <- stringr::str_extract(
    github.url, 
    paste0(
      "(?<=", 
      stringr::str_extract(
        github.url, 
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
  
  cm_dev
  
}




#' Join \code{codemeta} of R Package Description and GitHub
#'
#' @param codemeta.rpkg
#'   (codemeta list) \code{codemeta} of R Package Description
#' @param codemeta.github 
#'   (codemeta list) \code{codemeta} of GitHub
#'
#' @return
#'   (codemta list) \code{codemeta} list object
#'   
join_rpkg_github <- function(cm.rpkg, cm.github) {
  
  xwalk <- codemetar::crosswalk_table("R Package Description", "GitHub")
  
  # Set preferences for shared cross walk items
  xwalk$GitHub[xwalk$GitHub == "html_url"] <- NA_character_
  xwalk$GitHub[xwalk$GitHub == "login"] <- NA_character_
  xwalk$GitHub[xwalk$GitHub == "id"] <- NA_character_
  xwalk$GitHub[xwalk$GitHub == "full_name"] <- NA_character_
  
  # Left join
  cm.rpkg <- purrr::compact(cm.rpkg)
  cm.github <- purrr::compact(cm.github)
  use_i <- names(cm.github) %in% names(cm.rpkg)
  cm_joined <- c(cm.rpkg, cm.github[!use_i])
  
  cm_joined

}




#' Add codemeta to ontosoft metadata
#'
#' @param ontosoft
#'   (ontosoft list) OntoSoft json list object
#' @param cm
#'   (codemeta list) \code{codemeta} json list object
#'   
#' @details
#'   Not all OntoSoft properties are implemented due to unique aspects of 
#'   the IMCR use case, namely \code{hasSoftwareCategory} 
#'   (this is reserved for IMCR Vocabulary Terms), \code{hasDependency}
#'   (adds these software dependencies to the IMCR, which may or may not be 
#'   within the IMCR scope), \code{requiresAverageMemory} (which isn't 
#'   clearly supported by the OntoSoft API), \code{hasVersionReleaseDate},
#'   (which is listed in the ontology but doesn't seem to be implemented), and
#'   \code{hasName} (manually input by person registering the software).
#'   \code{usedInPublication} is supported by the crosswalk but not yet 
#'   implemented in this function.
#'
#' @return
#'   (ontosoft list) OntoSoft json list object
#'   
add_codemeta_to_ontosoft <- function(ontosoft, cm) {
  
  # hasActiveDevelopment
  val <- cm$codeRepository
  if (is.null(val)) {
    val <- cm$developmentStatus
  }
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#DevelopmentInformation',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasActiveDevelopment']] <- df
  }
  
  # hasImplementationLanguage 
  # FIXME: Format varies by metadata schema. Can be more than one.
  val <- cm$programmingLanguage$name
  if (!is.null(val)) {
    df <- data.frame(
      rep('EnumerationEntity', length(val)),
      rep('', length(val)),
      rep('', length(val)),
      rep('http://ontosoft.org/software#ProgrammingLanguage', length(val)),
      rep(NA, length(val)),
      val,
      stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasImplementationLanguage']] <- df
  }
  
  # supportsOperatingSystem
  val <- cm$operatingSystem
  if (!is.null(val)) {
    df <- data.frame(
      rep('EnumerationEntity', length(val)),
      rep('', length(val)),
      rep('', length(val)),
      rep('http://ontosoft.org/software#OperatingSystem', length(val)),
      rep(NA, length(val)),
      val,
      stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#supportsOperatingSystem']] <- df
  }
  
  # hasCommitmentOfSupport
  val <- cm$softwareHelp
  if (is.null(val)) {
    val <- cm$softwareHelp
  }
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasCommitmentOfSupport']] <- df
  }
  
  # hasSoftwareVersion
  # FIXME: Performing a PUT operation to this property results in 
  # erratic behavior.
  # val <- c("2.2.2", "2.1.0")
  # val <- cm$softwareVersion
  # if (!is.null(val)) {
  #   df <- data.frame(
  #     rep('EnumerationEntity', length(val)),
  #     rep('', length(val)),
  #     rep(NA, length(val)),
  #     rep('http://ontosoft.org/software#SoftwareVersion', length(val)),
  #     val,
  #     val,
  #     stringsAsFactors = FALSE
  #   )
  #   names(df) <- c("@type", "id", "name", "type", "label", "value")
  #   ontosoft$value[['http://ontosoft.org/software#hasSoftwareVersion']] <- df
  # }

  # hasRelevantDataSources
  val <- cm$supportingData
  if (!is.null(val)) {
    df <- data.frame(
      rep('TextEntity', length(val)),
      rep('', length(val)),
      rep('', length(val)),
      rep('http://ontosoft.org/software#TextEntity', length(val)),
      rep(NA, length(val)),
      val,
      stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasRelevantDataSources']] <- df
  }
  
  # hasCreator
  # FIXME: Input formats may vary. Creator can also come from codemeta "creator".
  val <- cm$author
  val <- unlist(
    lapply(
      seq_along(val),
      function(x) {
        paste0(val[[x]]$givenName, " ", val[[x]]$familyName)
      }
    )
  )
  if (!is.null(val)) {
    df <- data.frame(
      rep('EnumerationEntity', length(val)),
      rep('', length(val)),
      rep('', length(val)),
      rep('http://www.w3.org/ns/prov#Agent', length(val)),
      rep(NA, length(val)),
      val,
      stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasCreator']] <- df
  }
  
  # hasMajorContributor
  # FIXME: Input formats may vary.
  val <- cm$contributor
  val <- unlist(
    lapply(
      seq_along(val),
      function(x) {
        paste0(val[[x]]$givenName, " ", val[[x]]$familyName)
      }
    )
  )
  if (!is.null(val)) {
    df <- data.frame(
      rep('EnumerationEntity', length(val)),
      rep('', length(val)),
      rep('', length(val)),
      rep('http://www.w3.org/ns/prov#Agent', length(val)),
      rep(NA, length(val)),
      val,
      stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasMajorContributor']] <- df
  }
  
  # hasFundingSources
  # FIXME: Can also originate from cm$funding, which has
  # equal weight as cm$funder.
  # FIXME: Performing a PUT operation to this property results in 
  # erratic behavior.
  # Does the property need to be cleared first?
  # val <- c("It has these funding sources", "and these too")
  # val <- cm$funder
  # if (!is.null(val)) {
  #   df <- data.frame(
  #     rep('TextEntity', length(val)),
  #     rep('', length(val)),
  #     rep('', length(val)),
  #     rep('http://ontosoft.org/software#TextEntity', length(val)),
  #     rep(NA, length(val)),
  #     val,
  #     stringsAsFactors = FALSE
  #   )
  #   names(df) <- c("@type", "id", "name", "type", "label", "value")
  #   ontosoft$value[['http://ontosoft.org/software#hasFundingSources']] <- df
  # }
  
  # hasDomainKeywords
  # FIXME: Can be more than one.
  # FIXME: Performing a PUT operation to this property results in 
  # erratic behavior.
  # TESTME FAILED
  # val <- c("Limnology", "Oceanography")
  # val <- cm$keywords
  # if (!is.null(val)) {
  #   df <- data.frame(
  #     rep('TextEntity', length(val)),
  #     rep('', length(val)),
  #     rep('', length(val)),
  #     rep('http://ontosoft.org/software#Keywords', length(val)),
  #     rep(NA, length(val)),
  #     val,
  #     stringsAsFactors = FALSE
  #   )
  #   names(df) <- c("@type", "id", "name", "type", "label", "value")
  #   ontosoft$value[['http://ontosoft.org/software#hasDomainKeywords']] <- df
  # }
  
  # hasLicense
  # FIXME: Parse out license name if possible
  val <- cm$license
  if (!is.null(val)) {
    df <- data.frame(
      rep('EnumerationEntity', length(val)),
      rep('', length(val)),
      rep('', length(val)),
      rep('http://ontosoft.org/software#License', length(val)),
      rep(NA, length(val)),
      val,
      stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasLicense']] <- df
  }
  
  # hasPublisher
  val <- cm$publisher
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://ontosoft.org/software#Publisher',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasPublisher']] <- df
  }
  
  # hasShortDescription
  val <- cm$description
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      val, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasShortDescription']] <- df
  }
  
  # hasUniqueId
  val <- cm$identifier
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#Identifier',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasUniqueId']] <- df
  }

  # hasEmailContact
  # FIXME: Can be more than one
  # FIXME: Performing a PUT operation to this property results in 
  # erratic behavior.
  # val <- c("emailcontact@email.com", "anothercontact@email.com")
  # val <- cm$email
  # if (!is.null(val)) {
  #   df <- data.frame(
  #     rep('TextEntity', length(val)),
  #     rep('', length(val)),
  #     rep('', length(val)),
  #     rep('http://ontosoft.org/software#TextEntity', length(val)),
  #     rep(NA, length(val)),
  #     val,
  #     stringsAsFactors = FALSE
  #   )
  #   names(df) <- c("@type", "id", "name", "type", "label", "value")
  #   ontosoft$value[['http://ontosoft.org/software#hasEmailContact']] <- df
  # }

  # hasInstallationInstructions
  val <- cm$buildInstructions
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    ontosoft$value[['http://ontosoft.org/software#hasInstallationInstructions']] <- df
  }
  
  ontosoft
  
}

