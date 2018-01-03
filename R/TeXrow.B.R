#' TeX row (B)
#'
#' Generates a row of TeX code for the payoffs of Player B. For use in \code{\link{normalTexGame}}.
#'
#' @param rowB A row of payoffs of Player B
#' @return A string of TeX output
#'
#' @export

TeXrow.B <-
function(rowB){
  str <- paste(rowB, sep = ":", collapse="} & & \\multicolumn{1}{c|}{")
  str <- paste("& \\multicolumn{1}{c|}{",str,"} \\\\ ", sep = "")
}
