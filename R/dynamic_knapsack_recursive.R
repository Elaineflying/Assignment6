


dynamic_knapsack_recursive <- function(x,W) {
  # Checking x argument
  if ( !(is.data.frame(x) && any(x > 0) && all(names(x) %in% c("w", "v"))) ) {
    stop("The argument x should be a data frame with two variables v and w, with only positive values")
  }
  # Checking W argument
  if ( !(is.numeric(W) && W > 0)) {
    stop("The argument W should be a postive integer, please check!")
  }
  num_items <- nrow(x)

  # create a hash table to store calculated max values
  #hash_table <- hash()
  x_mat <- matrix(-1, nrow = num_items + 1, ncol = W + 1)

  # create a recursive function search the max values recursively
  knapsack_recursive <- function(i, j) {

    if (i == 0 || j <= 0) {
      return(0)
    }

    if (x_mat[i, j] != -1) {
      return(x_mat[i, j])
    }

    if (x$w[i] > j) {
      result <- knapsack_recursive(i - 1, j)
    } else {
      result <- max(
        knapsack_recursive(i - 1, j),
        knapsack_recursive(i - 1, j - x$w[i]) + x$v[i])
    }
    x_mat[i,j] <- result
    return(result)
  }
  # call recursive function to get the final results
  final_result <- knapsack_recursive(num_items,W)
  #return(list(value=round(final_result$value), elements=which(final_result$elements==1)))
  return(final_result)
}
