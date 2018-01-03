#' Dominant strategy game generator
#'
#' Create a game with a unique equilibrium in dominant strategies
#'
#' @param M Number of strategies of Player A (rows)
#' @param N Number of strategies of Player B (columns)
#' @param min Smallest payoff
#' @param max Biggest payoff
#' @return An object of class "Game"
#' @examples
#' example_game <- domGame(4,3,0,30)
#'
#' @export

domGame <-
function(M,N,min,max){
  A <- matrix(nrow = M, ncol = N)
  B <- A
  # Random index of dominant strategy
  dom.A <- sample(1:M,1)
  dom.B <- sample(1:N,1)

  sol.A <- rep(0,M)
  sol.B <- rep(0,N)

  sol.A[dom.A] <- 1
  sol.B[dom.B] <- 1

  # Fill out payoffs of dominant strategy
  A[dom.A,] <- sample((min+1):max,N, replace = T)
  B[,dom.B] <- sample((min+1):max,M, replace = T)
  # Fill out remaining payoffs
  for(i in 1:N){
    A[-dom.A,i] <- sample(min:(A[dom.A,i]-1),(M-1), replace = T)
  }

  for(j in 1:M){
    B[j,-dom.B] <- sample(min:(B[j,dom.B]-1),(N-1), replace = T)
  }
  game <- methods::new("Game", type="Dominant", M=M, N=N, payoff.A = A, payoff.B = B,
              sol.A = list(sol.A), sol.B = list(sol.B), sol.full = data.frame(A=dom.A, B=dom.B))
  return(game)

}
