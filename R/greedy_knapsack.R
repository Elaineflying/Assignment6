#' greedy search is to use the heurisitc or approximation for the knapsack problem.
#' @references Reference page link <https://en.wikipedia.org/wiki/Knapsack problem#Greedy approximation algorithm>
#' @description greedy search will not give an exact result, but it will reduce the computational complexity considerably.
#' @param x a data frame which contains two variables v and w, stands for each items value and weight respectively.
#' @param W a postive integer which stands for the knapsack size
#' @returns the maximum knapsack value and which elements.
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#' @export greedy_knapsack
# Greedy knapsack algorithm
greedy_knapsack <- function(x,W){

  # Check for inputs
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0) )
  stopifnot(W > 0)

  # initialization
  total_weight <- 0
  total_value <- 0
  elements <- c()

  # Remove all the values before hand whose weights are greater than the given Max weight W
  #discard <- which(x$w > W)
  #if(length(discard)>0){
  #  x <- x[-1, ]
  #  #for (i in 1:length(discard)) {
  #  #  x <- x[-i,]
  #  #}
  #}

  # Need ration of v/w
  x$p_val <- x$v/x$w
  x <- x[order(x$p_val, decreasing=TRUE), ]


  for(i in 1:nrow(x)){

    w <- total_weight + x$w[i]

    if(w <= W){
      total_weight <- w
      total_value <- total_value + x$v[i]
      elements <- c(elements, rownames(x[i, ]))
    }
    else if (w > W)(
      return(list(value = round(total_value, 0), elements = as.numeric(elements)))
    )
    if(i == nrow(x)) {
      return(list(value = round(total_value, 0), elements = as.numeric(elements)))
    }
  }

}
