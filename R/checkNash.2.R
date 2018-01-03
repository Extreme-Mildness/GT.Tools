#' Pure Nash checker (special)
#'
#' Find all Nash equilibria (for use in the function \code{\link{nashGame}})
#'
#' @param M Number of rows
#' @param N Number of columns
#' @param A Payoff matrix of Player A
#' @param B Payoff matrix of Player B
#' @return A data frame of Nash Equilibria
#'
#' @export

checkNash.2 <-
function(M,N,A,B){

  BR <- data.frame(row.ind=numeric(), col.ind=numeric())

  max.A <- apply(A,2,allmax)
  max.B <- apply(B,1,allmax)

  for(i in 1:length(max.A)){
    BR <- rbind(BR,cbind(max.A[[i]],rep(i,length(A[[i]]))))
  }

  for(j in 1:length(max.B)){
    BR <- rbind(BR,cbind(rep(j,length(B[[j]])),max.B[[j]]))
  }


  NE <- BR[duplicated(BR),]
  colnames(NE) <- c("A","B")
  rownames(NE) <- NULL
  return(NE)
}
