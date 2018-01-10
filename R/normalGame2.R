#' Normal form representation (for Shiny)
#'
#' Creates a normal form representation viewable as an R matrix.
#' To ensure compatibility with Shiny tables, each outcome is represented
#' by a single cell, with the payoffs of A and B separated by a comma.
#'
#' @param game An object of class "Game"
#' @return A matrix with a normal form representation.
#' @examples
#' example_game <- nashGame(4,3,0,30)
#' normal_form <- normalGame2(example_game)
#'
#' @export

normalGame2 <-
  function(game){
    normal <- matrix(paste(game@payoff.A,game@payoff.B, sep = ","),
                        game@M, game@N)
    colnames(normal) <- 1:game@N

    return(normal)
  }
