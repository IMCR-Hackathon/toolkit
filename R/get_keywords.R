#' List kewords
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
list_keywords <- function(json){
  return(
    lapply(
      seq_along(json),
      function(x){
        json[[x]]$value[['http://ontosoft.org/software#hasSoftwareCategory']]$label
      }
    )
  )
}
