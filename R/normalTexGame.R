#' TeX output of a game
#'
#' Creates a normal form representation of a game and exports it
#' in LaTeX format. Can also display pure strategy Nash equilibria
#' as well as solutions adapted to each type of game. By default, exports
#' to console, but can also write to a TeX file.
#'
#' @param game An object of class "Game"
#' @param showOutput A boolean to output the text to console. Overridden by \code{fileOut}. Defaults to \code{TRUE}.
#' @param showNash A boolean to switch on underlining of pure strategy Nash equilibria. Defaults to \code{FALSE}.
#' @param showSol A boolean to output the solution text. Defaults to \code{FALSE}.
#' @param fileOut A boolean to write directly to a TeX file. Defaults to \code{FALSE}.
#' @return A character vector with each cell being one line of the TeX code which can be outputted
#' with \code{writeLines()}. By default, the output is printed directly to the console.
#'
#' @examples
#' example_game <- nashGame(4,3,0,30)
#' normalTexGame(example_game, showNash = TRUE)
#' normalTexGame(example_game, showSol = TRUE, fileOut = TRUE)
#'
#' @export

normalTexGame <-
function(game, showOutput = TRUE, showNash = FALSE, showSol = FALSE, fileOut = FALSE){
  A <- as.character(MASS::fractions(game@payoff.A))
  B <- as.character(MASS::fractions(game@payoff.B))

  M <- game@M
  N <- game@N

  # Underline payoffs of Nash equilibria
  if(showNash){
    Nash <- checkNash(game)
    for(i in 1:nrow(Nash)){
      A[Nash[i,1],Nash[i,2]] <- paste("\\underline{", A[Nash[i,1],Nash[i,2]], "}", sep = "")
      B[Nash[i,1],Nash[i,2]] <- paste("\\underline{", B[Nash[i,1],Nash[i,2]], "}", sep = "")
    }
  }

  # Begin the table
  begin <- c("\\begin{table}[!htbp]", "\\centering")
  begin[3] <- paste("\\begin{tabular}{", paste(rep("c",N*2+2),collapse=""),"}", sep="")

  # Label for B and strategies
  labelB <- paste("& & \\multicolumn{",N*2,"}{c}{Player B} \\\\", sep = "")
  labelB[2] <- paste("& ", paste("& \\multicolumn{2}{c}{", 1:N, "} ", sep="", collapse = ""), "\\\\ \\cline{3-", N*2+2, "}", sep = "")

  # Fill in payoffs
  rowA <- apply(A, 1, TeXrow.A)
  rowA <- paste("& \\multicolumn{1}{c|}{} & ", rowA, sep = "")

  rowB <- apply(B, 1, TeXrow.B)
  rowB <- paste("& \\multicolumn{1}{c|}{\\multirow{2}{*}{",1:M,"}} & ", rowB, sep = "")

  payoffs <- character(M*2)
  payoffs[2*1:M] <- rowA
  payoffs[2*1:M-1] <- rowB

  payoffs[1] <- paste("\\multirow{", M*2,"}{*}{\\rotatebox[origin=c]{90}{Player A}} ", payoffs[1], sep="")

  # End the table
  end <- c("\\end{tabular}","\\end{table}")

  # Solutions
  if(showSol){
    solText <- "\\textit{\\Large Solution:} \\\\"

    if(game@type=="PureNash"){
      sol <- game@sol.full
      if(nrow(sol)>1){
        solText[2] <- paste("The pure Nash equilibria are ",paste("$(",sol[,1],", ", sol[,2],")$", sep = "", collapse = ", "),".")
      } else {
        solText[2] <- paste("The pure Nash equilibria are ",paste("$(",sol[,1],", ", sol[,2],")$", sep = "", collapse = ", "),".")
      }
    }

    if(game@type=="Dominant"){
      sol <- game@sol.full
      solText[2] <- paste("The dominant strategy pair is $(", sol[,1], ", ", sol[,2],")$.", sep = "")
    }

    if(game@type=="IESDS"){
      sol <- game@sol.full
      solText <- c(solText, paste(1:nrow(sol), ". Eliminate strategy ", sol[,2], " of Player ", sol[,1], ". \\\\", sep=""))
    }

    if(game@type=="Mixed"){
      sol.A <- game@sol.A[[1]]
      sol.B <- game@sol.B[[1]]
      solText[2] <- paste("A possible mixed strategy of player A is $\\{", paste(sol.A, sep="", collapse = ", "), "\\}$. \\\\")
      solText[3] <- paste("A possible mixed strategy of player B is $\\{", paste(sol.B, sep="", collapse = ", "), "\\}$. \\\\")
    }

    end <- c(end, solText)
  }


  fullTex <- c(begin, labelB, payoffs, end)

  if(fileOut){
    fileConn<-file(paste("game_", substitute(game), ".tex", sep = ""))
    writeLines(fullTex, fileConn)
    close(fileConn)
  } else if(showOutput) {
    writeLines(fullTex)
  } else {
    return(fullTex)
  }
}
