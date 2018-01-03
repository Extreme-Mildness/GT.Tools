#' Nash game generator
#'
#' Create a game with one or more pure strategy Nash equilibria
#'
#' @param M Number of strategies of Player A (rows)
#' @param N Number of strategies of Player B (columns)
#' @param min Smallest payoff
#' @param max Biggest payoff
#' @return An object of class "Game"
#' @examples
#' example_game <- nashGame(4,3,0,30)
#'
#' @export

nashGame <-
function(M,N,min,max){
  A <- matrix(nrow = M, ncol = N)
  B <- A

  Nequi <- sample(1:min(M,N),1)

  ind.A <- sample(1:M,Nequi)
  ind.B <- sample(1:N,Nequi)


  for(i in 1:Nequi){
    val.A <- sample(min:max,1)
    A[ind.A[i],ind.B[i]] <- val.A
    A[-ind.A[i],ind.B[i]] <- sample(min:(val.A-1),M-1, replace = T)

    val.B <- sample(min:max,1)
    B[ind.A[i],ind.B[i]] <- val.B
    B[ind.A[i],-ind.B[i]] <- sample(min:(val.B-1),N-1, replace = T)

    # sol.A[[i]][ind.A[i]] <- 1
    # sol.B[[i]][ind.B[i]] <- 1
  }

  A[,-ind.B] <- sample(min:max,(N-Nequi)*M, replace = T)
  B[-ind.A,] <- sample(min:max,(M-Nequi)*N, replace = T)

  sol <- checkNash.2(M,N,A,B)

  sol.A <- rep(list(rep(0,M)),nrow(sol))
  sol.B <- rep(list(rep(0,N)),nrow(sol))

  for(k in 1:nrow(sol)){
    row <- sol[k,1]
    col <- sol[k,2]
    sol.A[[k]][row] <- 1
    sol.B[[k]][col] <- 1
  }

  game <- methods::new("Game", type="PureNash", M=M, N=N, payoff.A=A, payoff.B=B, sol.A=sol.A, sol.B=sol.B, sol.full=sol)

}
