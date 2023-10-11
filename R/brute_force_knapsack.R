#' brute-force search is a algorithm which is guaranteed to give a correct answer in all situations for the knapsack problem.
#' @references Reference page link <https://dataxujing.github.io/R_oop/RC.html#section-5.5.1>
#' @description brute_force search is going through all possible alternatives and return the maximum value found.
#' @param x a data frame which contains two variables v and w, stands for each items value and weight respectively.
#' @param W a postive integer which stands for the knapsack size
#' @returns the maximum knapsack value and which elements.
#' @examples 
#' RNGversion(min(as.character(getRversion()),"3.5.3"))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <-
#' data.frame(
#' w=sample(1:4000, size = n, replace = TRUE),
#' v=runif(n = n, 0, 10000)
#' )
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' @import plyr
#' @import ggplot2
#' @import methods
#' @export brute_force_knapsack
brute_force_knapsack <-
function(x,w) {
    return(value)
    return(elements)  
}
