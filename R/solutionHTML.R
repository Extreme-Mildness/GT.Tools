#' HTML output of a game solution
#'
#' Writes the solution of the game according to game type with html
#' formatting. Primarily intended for use in shiny.
#'
#' @param game An object of class "Game"
#'
#' @examples
#' example_game <- nashGame(4,3,0,30)
#' solutionHTML(example_game)
#'
#' @export

solutionHTML <- function(game){
  solText <- "<h4>Solution:</h4> <br>\n"
  if(game@type=="PureNash"){
    sol <- game@sol.full
    if(nrow(sol)>1){
      solText[2] <- paste("The pure Nash equilibria are ",paste("<b>(",sol[,1],", ", sol[,2],")</b>", sep = "", collapse = ", "),".")
    } else {
      solText[2] <- paste("The pure Nash equilibrium is ",paste("<b>(",sol[,1],", ", sol[,2],")</b>", sep = "", collapse = ", "),".")
    }
  }

  if(game@type=="Dominant"){
    sol <- game@sol.full
    solText[2] <- paste("The dominant strategy pair is <b>(", sol[,1], ", ", sol[,2],")</b>.", sep = "")
  }

  if(game@type=="IESDS"){
    sol <- game@sol.full
    solText <- c(solText, paste(1:nrow(sol), ". Eliminate strategy <b>", sol[,2], "</b> of Player <b>", sol[,1], "</b>. <br>\n", sep=""))
  }

  if(game@type=="Mixed"){
    sol.A <- game@sol.A[[1]]
    sol.B <- game@sol.B[[1]]
    solText[2] <- paste("A possible mixed strategy of player A is <b>{", paste(sol.A, sep="", collapse = ", "), "}</b>. <br>\n")
    solText[3] <- paste("A possible mixed strategy of player B is <b>{", paste(sol.B, sep="", collapse = ", "), "}</b>. <br>\n")
  }

  return(solText)
}
