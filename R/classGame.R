#' New class for games
#'
#' @param type A string to describe the game
#' @param M Number of strategies of Player A (rows)
#' @param N Number of strategies of Player B (columns)
#' @param payoff.A A numerical matrix of payoffs of Player A
#' @param payoff.B A numerical matrix of payoffs of Player B
#' @param sol.A A list of vectors of strategies of Player A
#' @param sol.B A list of vectors of strategies of Player B
#' @param sol.full A dataframe of game solutions, formatted according to game type
#'
#' @export

setClass("Game", representation(type="character", M = "numeric", N = "numeric",
                                payoff.A = "matrix", payoff.B = "matrix",
                                sol.A = "list", sol.B = "list", sol.full = "data.frame"))
