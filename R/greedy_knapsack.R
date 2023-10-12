#' greedy search is to use the heurisitc or approximation for the knapsack problem.
#' @references Reference page link <https://en.wikipedia.org/wiki/Knapsack problem#Greedy approximation algorithm>
#' @description greedy search will not give an exact result, but it will reduce the computational complexity considerably.
#' @param x a data frame which contains two variables v and w, stands for each items value and weight respectively.
#' @param W a postive integer which stands for the knapsack size
#' @returns the maximum knapsack value and which elements.
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#' @import plyr
#' @import ggplot2
#' @import methods
#' @export greedy_knapsack
# Greedy knapsack algorithm
greedy_knapsack <- function(x, W) {

  # Checking x argument
  if ( !(is.data.frame(x) && any(x > 0)) ) {
    stop("The argument x should be a data frame with two variables v and w, with only positive values")
  }
  # Checking W argument
  if ( !(is.numeric(W) && W > 0)) {
    stop("The argument W should be a postive integer, please check!")
  }
  # Sort the items in decreasing order of value-to-weight ratio.
  x <- x[order(x[, 2] / x[, 1], decreasing = TRUE), ]
  value<-0
  elements<-integer(0)
  weight <- 0

  # Initialize the knapsack.
  knapsack <- list(value=value, elements=elements)

  # Iterate over the items in sorted order.
  for (i in 1:nrow(x)) {
    item <- x[i, ]

    # Choosing integer values for if loop
    item_weight <- as.integer(item[1])
    item_value <- as.integer(item[2])

    # If the item fits in the knapsack, add it.
    if (item_weight + weight <= W) {
      knapsack$value <- knapsack$value + item_value  # Store the values
      knapsack$elements <- c(knapsack$elements, i)  # Store the index of the item.
      weight <- weight + item_weight   # Store the weights
    }
  }

  return(knapsack)
}

