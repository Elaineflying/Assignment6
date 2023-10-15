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
#' @export brute_force_knapsack
brute_force_knapsack <- function(x,W) {
  # Checking x argument
  if ( !(is.data.frame(x) && any(x > 0) && all(names(x) %in% c("w", "v"))) ) {
    stop("The argument x should be a data frame with two variables v and w, with only positive values")
  }
  # Checking W argument
  if ( !(is.numeric(W) && W > 0)) {
    stop("The argument W should be a postive integer, please check!")
  }
  num_items <- nrow(x)
  max_value <- 0
  best_elements <-numeric(num_items)

  # Generate all possible binary combination for item
  for ( i in 0:(2^num_items -1)) {
    binary_search <- as.integer(intToBits(i))[1:num_items]
    if ( sum(binary_search * x$w) <= W ) {
      current_value <- sum(binary_search * x$v)
      if (current_value > max_value) {
        max_value <- current_value
        best_elements <- binary_search
      }
    }
  }
  value <- round(max_value)
  elements <- which(best_elements==1)
  return(list(value=value, elements=elements))
}
