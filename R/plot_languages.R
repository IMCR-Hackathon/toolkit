#' Plot software languages
#'
#' @param languages
#'   (list) Software languages
#'
#' @return
#'   (.png) Pie chart of input implementation languages
#'
#' @export
#'
#' @examples
#' \dontrun{
#' json <- get_json('http://imcr.ontosoft.org/repository/software')
#' list_creators(json)
#' }
#'
plot_languages <- function(languages){
  dfc <- plyr::count(unlist(languages))
  other <- sum(dfc$freq[(dfc$x != 'Python') & (dfc$x != 'R')])
  dfc <- data.frame(
    x = c('R', 'Python', 'Other'),
    freq = c(dfc$freq[dfc$x == 'R'], dfc$freq[dfc$x == 'Python'], other),
    stringsAsFactors = FALSE
  )
  dfc$freq <- (dfc$freq/sum(dfc$freq))*100
  colnames(dfc) <- c("class", "freq")
  dfc$class <- factor(dfc$class, levels = rev(as.character(dfc$class)))
}




#' List implementation languages
#'
#' @param json
#'   (list) Software metadata in JSON format
#'
#' @return
#'   (list) Software languages
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
