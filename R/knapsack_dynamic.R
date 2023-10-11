#' Dynamic search should return the same results as the brute force algorithm when solving the knapsack problem.
#' @references Reference page link <https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem>
#' @description Dynamic search return the same results as brute force algorithm, but it should scale much better with O(W n) runing speed.
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
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @import plyr
#' @import ggplot2
#' @import methods
#' @export knapsack_dynamic
knapsack_dynamic <-
function(x,w) {
    return(value)
    return(elements)
}
