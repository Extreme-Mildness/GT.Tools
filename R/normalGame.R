#' Normal form representation
#'
#' Creates a normal form representation viewable as an R matrix.
#' To make the game easier to read, each row and column of the original
#' payoff matrices are duplicated. The payoffs of A are then in the bottom left
#' of each 2x2 cell, and the payoffs of B are in the top right.
#'
#' @param game An object of class "Game"
#' @return A matrix with a normal form representation.
#' @examples
#' example_game <- nashGame(4,3,0,30)
#' normal_form <- normalGame(example_game)
#'
#' @export

normalGame <-
function(game){
  M <- game@M
  N <- game@N

  normal <- matrix(nrow=M*2, ncol=N*2)

  Modd <- seq(1,M*2,by=2)
  Meven <- seq(2,M*2,by=2)
  Nodd <- seq(1,N*2,by=2)
  Neven <- seq(2,N*2,by=2)

  normal[Meven,Nodd] <- game@payoff.A
  normal[Modd,Neven] <- game@payoff.B

  return(normal)
}
