#' Set Software Category
#'
#' Add or remove software category keywords.
#'
#' @param software
#'   (character) Software name
#' @param keywords
#'   (character) New keywords
#' @param action
#'   (character) "add" or "remove" software category
#' @param json
#'   (list) JSON of all OntoSoft Portal software. Create this list with
#'   \code{get_json()}.
#' @param session.string
#'   (character) Key generated with \code{login()}.
#'
#' @return
#'   (json) Updated JSON object and a JSON file written to \code{tempdir()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_keyword('arrow', c('quality control', 'import'))
#' }
#'
set_SoftwareCategory <- function(software, keywords, action, json, session.string){

  metadata <- json[names(json) == software]
  categories <- metadata[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']]

  metadata <- json[names(json) == "R packages: parsedate  and methods are required for qaqcTimeDate_functions.R "]

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
    names(new_categories) <- names(categories)
    categories <- rbind(categories, new_categories)
    metadata[[1]]$value[['http://ontosoft.org/software#hasSoftwareCategory']] <- categories
  } else if (action == 'remove'){

  }

  metadata <- json[names(json) == software]
  metadata[[1]]$value[['http://ontosoft.org/software#hasShortDescription']]
  # metadata[[1]]$provenance <- list()
  metadata[[1]]$provenance <- NULL
  # metadata[[1]]$permission <- list()
  metadata[[1]]$permission <- NULL
  metadata[[1]]$id <- ''
  metadata[[1]]$name <- ''
  # write to file
  jsonlite::write_json(
    metadata[[1]],
    path = '/Users/csmith/Desktop/testing.json',
    auto_unbox = TRUE
  )
  # validate
  jsonlite::validate(jsonlite::toJSON(metadata[[1]]))

  # upload to imcr
  r <- httr::PUT(
    url = metadata[[1]]$id,
    body = upload_file(paste0(tempdir(), '/ontosoftjson.JSON')),
    add_headers(`X-Ontosoft-Session` = session.string)
  )

  return(ids)
}
