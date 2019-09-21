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
          if (isTRUE(stringr::str_detect(url_dev, "github"))) {
            cm_dev <- gh_to_codemeta(url_dev)
          }
        }
        
      } else if (isTRUE(stringr::str_detect(url, "github"))) {
        
        cm_src <- gh_to_codemeta(url)
        
      }
      
      # Join codemeta of source archive and development repository.
      if (exists("cm_src") & exists("cm_dev")) {
        
        if (stringr::str_detect(url, "cran") & 
            stringr::str_detect(url_dev, "github")) {
          cm <- join_rpkg_github(cm_src, cm_dev)
        }
        
      } else if (exists("cm_src") & !exists("cm_dev")) {
        
        cm <- purrr::compact(cm_src)
        
      }
      
      # Add codemeta to OntoSoft
      json <- add_codemeta_to_ontosoft(json, cm)
      
      # Update the imcr_json and imcr_json_mod_index objects
      # imcr_json[names(imcr_json) == name[i]][[1]] <<- json
      imcr_json[names(imcr_json) == name[i]][[1]] <- json
      # imcr_json_mod_index[names(imcr_json) == name[i]] <<- TRUE
      imcr_json_mod_index[names(imcr_json) == name[i]] <- TRUE

    }

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
#' @param codemeta 
#'   (codemeta list) \code{codemeta} json list object
#'
#' @return
#'   (ontosoft list) OntoSoft json list object
#'   
add_codemeta_to_ontosoft <- function(ontosoft, codemeta) {
  
  # hasActiveDevelopment
  # FIXME: Value can also originate from cm$developmentStatus, but is less preferred
  # than cm$codeRepository
  val <- cm$codeRepository
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#DevelopmentInformation',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasActiveDevelopment']] <- df
  }
  
  # hasImplementationLanguage 
  # FIXME: Format varies by metadata schema
  val <- cm$programmingLanguage$name
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://ontosoft.org/software#ProgrammingLanguage',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasImplementationLanguage']] <- df
  }

  # hasSoftwareCategory (this is reserved for IMCR Vocabulary Terms)
  # hsc <- json$value[['http://ontosoft.org/software#hasSoftwareCategory']]$value
  
  # supportsOperatingSystem
  val <- cm$operatingSystem
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://ontosoft.org/software#OperatingSystem',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#supportsOperatingSystem']] <- df
  }
  
  # hasCommitmentOfSupport
  # FIXME: Can also originate from cm$issueTracker, which may or may not be
  # preferred to cm$softwareHelp
  val <- cm$softwareHelp
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasCommitmentOfSupport']] <- df
  }
  
  # hasDependency
  # DO NOT USE THIS, it adds these dependencies to the IMCR, and a lot
  # of dependencies don't belong in the IMCR.
  # val <- cm$softwareRequirements
  # if (!is.null(val)) {
  #   df <- data.frame(
  #     'EnumerationEntity', '', '', 'http://ontosoft.org/software#Software',
  #     NA, val, stringsAsFactors = FALSE
  #   )
  #   names(df) <- c("@type", "id", "name", "type", "label", "value")
  #   json$value[['http://ontosoft.org/software#hasDependency']] <- df
  # }
  
  # hasSoftwareVersion
  val <- cm$softwareVersion
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://ontosoft.org/software#SoftwareVersion',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasSoftwareVersion']] <- df
  }
  
  # requiresAverageMemory
  val <- cm$storageRequirements
  units <- NA
  if (!is.null(val)) {
    df <- data.frame(
      'MeasurementEntity', '', '', 'http://ontosoft.org/software#MeasurementEntity',
      NA, val, units, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value", "units")
    json$value[['http://ontosoft.org/software#requiresAverageMemory']] <- df
  }
  
  # hasRelevantDataSources
  # FIXME: Can have multiple sources.
  val <- cm$supportingData
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasRelevantDataSources']] <- df
  }
  
  # hasCreator
  # FIXME: Can have multiple creators. Input formats may vary. Creator can also come
  # from codemeta "creator".
  val <- cm$author
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://www.w3.org/ns/prov#Agent',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasCreator']] <- df
  }
  
  # hasMajorContributor
  # FIXME: Can have multiple contributors. Input formats may vary.
  val <- cm$contributor
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://www.w3.org/ns/prov#Agent',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasMajorContributor']] <- df
  }
  
  # hasVersionReleaseDate
  # FIXME: Does this property exist in OntoSoft? If so the "type" in the 
  # code below needs updating.
  # val <- cm$dateModified
  # if (!is.null(val)) {
  #   df <- data.frame(
  #     'EnumerationEntity', '', '', 'http://www.w3.org/ns/prov#Agent',
  #     NA, val, stringsAsFactors = FALSE
  #   )
  #   names(df) <- c("@type", "id", "name", "type", "label", "value")
  #   json$value[['http://ontosoft.org/software#hasVersionReleaseDate']] <- df
  # }
  
  # hasFundingSources
  # FIXME: Can be more than one. Can also originate from cm$funding, which has
  # equal weight as cm$funder.
  val <- cm$funder
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasFundingSources']] <- df
  }
  
  # hasDomainKeywords
  # FAILED
  # FIXME: Can be more than one.
  val <- cm$keywords
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#Keywords',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasDomainKeywords']] <- df
  }
  
  # hasLicense
  # FIXME: Can be more than one.
  val <- cm$license
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://ontosoft.org/software#License',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasLicense']] <- df
  }
  
  # hasPublisher
  val <- cm$publisher
  if (!is.null(val)) {
    df <- data.frame(
      'EnumerationEntity', '', '', 'http://ontosoft.org/software#Publisher',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasPublisher']] <- df
  }
  
  # hasShortDescription
  # FAILED
  val <- cm$description
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasShortDescription']] <- df
  }
  
  # hasUniqueId
  val <- cm$identifier
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#Identifier',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasUniqueId']] <- df
  }
  
  # hasName  (this is reserved for IMCR Vocabulary Terms)
  # json$value[['http://ontosoft.org/software#hasName']]

  # hasEmailContact
  # FIXME: Can be more than one
  val <- cm$email
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasEmailContact']] <- df
  }

  # hasInstallationInstructions
  val <- cm$buildInstructions
  if (!is.null(val)) {
    df <- data.frame(
      'TextEntity', '', '', 'http://ontosoft.org/software#TextEntity',
      NA, val, stringsAsFactors = FALSE
    )
    names(df) <- c("@type", "id", "name", "type", "label", "value")
    json$value[['http://ontosoft.org/software#hasInstallationInstructions']] <- df
  }
  
  # usedInPublication
  # FIXME: 2 parts to the publication. Can be more than one publication.
  # This has not yet been implemented...
  # val <- cm$referencePublication
  # if (!is.null(val)) {
  #   df <- data.frame(
  #     'ComplexEntity', '', '', 'http://ontosoft.org/software#Citation',
  #     NA, val, stringsAsFactors = FALSE
  #   )
  #   names(df) <- c("@type", "id", "name", "type", "label", "value")
  #   json$value[['http://ontosoft.org/software#usedInPublication']] <- df
  # }
  
}

