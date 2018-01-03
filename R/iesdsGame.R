#' IESDS game generator
#'
#' Create a game with a unique equilibrium found by IESDS
#'
#' @param M Number of strategies of Player A (rows)
#' @param N Number of strategies of Player B (columns)
#' @param min Approximate smallest payoff
#' @param max Approximate biggest payoff
#' @param maxdiff Maximum difference between a dominated strategy and the best strategy
#' @return An object of class "Game"
#' @examples
#' example_game <- iesdsGame(4,3,0,30,5)
#'
#' @export

iesdsGame <-
function(M,N,min,max,maxdiff){
  A <- matrix(sample(min:max,1))
  B <- matrix(sample(min:max,1))
  sol <- data.frame(Player=character(),Strat=numeric(),stringsAsFactors=FALSE)

  currM <- 1
  currN <- 1

  while(currM!=M|currN!=N){
    # rowcol is equal to 0 if the next addition is a row, 1 if it's a column
    if(nrow(A)==M){rowcol <- 1}
    else if(ncol(A)==N){rowcol <- 0}
    else {rowcol <- sample(0:1,1)}

    # add a row
    if(1-rowcol){
      sol[nrow(sol)+1,1] <- "A"
      upper <- apply(A,2,max)
      newA <- upper-sample(1:maxdiff,currN)
      newB <- sample(min:max,currN)
      if(sample(0:1,1)){
        A <- rbind(A,newA)
        B <- rbind(B,newB)
        sol[nrow(sol),2] <- currM+1
      } else {
        A <- rbind(newA,A)
        B <- rbind(newB,B)
        sol[sol$Player=="A",2] <- sol[sol$Player=="A",2]+1
        sol[nrow(sol),2] <- 1
      }

    }

    # add a column
    if(rowcol){
      sol[nrow(sol)+1,1] <- "B"
      upper <- apply(B,1,max)
      newB <- upper-sample(1:maxdiff,currM)
      newA <- sample(min:max,currM)
      if(sample(0:1,1)){
        A <- cbind(A,newA)
        B <- cbind(B,newB)
        sol[nrow(sol),2] <- currN+1
      } else {
        A <- cbind(newA,A)
        B <- cbind(newB,B)
        sol[sol$Player=="B",2] <- sol[sol$Player=="B",2]+1
        sol[nrow(sol),2] <- 1
      }
    }

    currM <- nrow(A)
    currN <- ncol(A)

  }
  game <- methods::new("Game", type="IESDS", M=M, N=N, payoff.A=A, payoff.B=B, sol.full=sol[rev(rownames(sol)),])
  return(game)
}
