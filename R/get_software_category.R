#' Get Software Category
#'
#' Get software category keywords.
#'
#' @param json
#'   (list) Software metadata in JSON format
#'
#' @return
#'   (list) Software keywords
#'
#' @export
#'
#' @examples
#' \dontrun{
#' json <- get_json('http://imcr.ontosoft.org/repository/software')
#' parse_keywords(json)
#' }
#'
get_software_category <- function(json){
  return(
    lapply(
      seq_along(json),
      function(x){
        json[[x]]$value[['http://ontosoft.org/software#hasSoftwareCategory']]$label
      }
    )
  )
}
