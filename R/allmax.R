#' Find all maxima
#'
#' Find indices of all numbers equal to the maximum in a vector
#'
#' @param vector A numerical vector
#' @return A vector of the indices of the maxima
#' @examples
#' allmax(c(5,3,-7,5))
#' allmax(1:10)
#'
#' @export

allmax <-
function(vector){
  return(which(vector==max(vector)))
}
