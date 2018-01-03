#' TeX row (A)
#'
#' Generates a row of TeX code for the payoffs of Player A. For use in \code{\link{normalTexGame}}.
#'
#' @param rowA A row of payoffs of Player A
#' @return A string of TeX output
#'
#' @export

TeXrow.A <-
function(rowA){
  str <- paste(rowA, sep = ":", collapse=" & \\multicolumn{1}{c|}{} & ")
  str <- paste(str," & \\multicolumn{1}{c|}{} \\\\ \\cline{3-",2*length(rowA)+2,"}", sep = "")
}
