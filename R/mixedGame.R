#' Mixed strategy game generator
#'
#' Create a game with at least one mixed strategy equilibrium
#'
#' @param M Number of strategies of Player A (rows) and Player B (columns)
#' @return An object of class "Game"
#' @examples
#' example_game <- mixedGame(4)
#'
#' @export

mixedGame <-
function(M){
  fracMax <- 5
  N <- M
  min <- 0
  max <- 10
  # Create mixed strategies
  num.A <- sample(1:fracMax,M,replace = T)
  num.B <- sample(1:fracMax,N,replace = T)
  strat.A <- num.A/sum(num.A)
  strat.B <- num.B/sum(num.B)

  payoff.A <- matrix(sample(min:max, M*N, replace = T), M, N)
  payoff.B <- matrix(sample(min:max, M*N, replace = T), M, N)


  # Fill out payoffs so as to satisfy mixed equilibrium
  c.A <- round(mean(payoff.A%*%strat.B))
  c.B <- round(mean(payoff.B%*%strat.A))
  payoff.A <- t(apply(payoff.A,1,mixedVec, strat=strat.B, c=c.A))
  payoff.B <- apply(payoff.B,2,mixedVec, strat=strat.A, c=c.B)

  game <- methods::new("Game", type="Mixed", M=M, N=N, payoff.A = payoff.A, payoff.B = payoff.B,
              sol.A = list(MASS::fractions(strat.A)), sol.B = list(MASS::fractions(strat.B)))

}
