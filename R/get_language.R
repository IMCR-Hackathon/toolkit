#' List implementation languages
#'
#' @param json
#'   (list) Software metadata in JSON format
#'
#' @return
#'   (list) Software languages
#'
#' @export
#'
#' @examples
#' \dontrun{
#' json <- get_json('http://imcr.ontosoft.org/repository/software')
#' list_languages(json)
#' }
#'
list_language <- function(json){
  return(
    lapply(
      seq_along(json),
      function(x){
        json[[x]]$value[['http://ontosoft.org/software#hasImplementationLanguage']]$label
      }
    )
  )
}
