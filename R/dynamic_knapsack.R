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
#' dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' @export dynamic_knapsack
dynamic_knapsack <- function(x,W) {
  # Checking x argument
  if ( !(is.data.frame(x) && any(x > 0) && all(names(x) %in% c("w", "v"))) ) {
    stop("The argument x should be a data frame with two variables v and w, with only positive values")
  }
  # Checking W argument
  if ( !(is.numeric(W) && W > 0)) {
    stop("The argument W should be a postive integer, please check!")
  }
  num_items <- nrow(x)
  #create a dynamic programming table to store the maximum values
  x_mat <- matrix(0, nrow = num_items + 1, ncol = W + 1)
  for (i in 1:num_items) {
    for ( j in 0:W) {
      if ( x$w[i] <= j) {
        x_mat[i + 1, j] <- max(
          x_mat[i, j],
          x_mat[i, j - x$w[i]] + x$v[i]
        )
      } else {
        x_mat[i + 1, j] <- x_mat[i, j]
      }
    }
  }
  # trace back to find the best elements
  best_elements <- numeric(num_items)
  i <- num_items
  j <- W
  while ( i > 0 && j > 0) {
    if (x_mat[i + 1,j] != x_mat[i, j]) {
      best_elements[i] <- 1
      j <- j - x$w[i]
    }
    i <- i - 1
  }
  value <- round(x_mat[num_items + 1, W])
  elements <- which(best_elements==1)
  return(list(value=value, elements=elements))
}
