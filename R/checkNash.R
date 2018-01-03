#' Pure Nash checker
#'
#' Find all Nash equilibria in a game
#'
#' @param game An object of class "Game"
#' @return A data frame of Nash Equilibria
#' @examples
#' example_game <- nashGame(4,3,0,30)
#' nashEq <- checkNash(example_game)
#'
#' @export

checkNash <-
function(game){
  M <- game@M
  N <- game@N

  A <- game@payoff.A
  B <- game@payoff.B

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
