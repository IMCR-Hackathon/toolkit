#' List software creators
#'
#' @param json
#'   (list) Software metadata in JSON format
#'
#' @return
#'   (list) Software creators
#'
#' @examples
#' \dontrun{
#' json <- get_json('http://imcr.ontosoft.org/repository/software')
#' list_creators(json)
#' }
#'
list_creators <- function(json){
  return(
    lapply(
      seq_along(json),
      function(x){
        json[[x]]$value[['http://ontosoft.org/software#hasCreator']]$label
      }
    )
  )
}
