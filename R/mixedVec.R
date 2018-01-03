#' Vector modification for mixedGame.R
#'
#' Given a vector of payoffs and the opponent's mixed strategy the
#' function modifies the payoffs so the expected payoff of the strategy equals
#' the given constant. Built for use in the function \code{\link{mixedGame}}.
#'
#' @param payoffVec A vector of payoffs, corresponding to a single pure strategy
#' @param strat The mixed strategy of the opponent
#' @param c The desired expected payoff
#' @return A modified vector of payoffs
#' @examples
#' mixedVec(c(-5, 3, 7), c(0.2, 0.5, 0.3), 4)
#'
#' @export

mixedVec <-
function(payoffVec, strat, c){
  ind <- sample(1:length(payoffVec),1)
  payoffVec[ind] <- (c-payoffVec[-ind]%*%strat[-ind])[1,1]/strat[ind]
  return(payoffVec)
}
