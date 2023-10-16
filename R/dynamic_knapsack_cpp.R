#' Dynamic search using Rcpp to solve the knapsack problem.
# Install and load the Rcpp package if not already installed
#if (!requireNamespace("Rcpp", quietly = TRUE)) {
#  install.packages("Rcpp")
#}
#library(Rcpp)
#' @improt Rcpp
#' @export dynamic_knapsack_cpp
cppFunction( code = '
List dynamic_knapsack_cpp (DataFrame x, int W) {
  NumericVector weights = x["w"];
  NumericVector values = x["v"];
  // Checking x argument
  if (x.containsElementNamed("w") && x.containsElementNamed("v")) {
    // Check if any element in weights is less than or equal to 0
    bool hasNegativeWeights = false;
    for (int i = 0; i < x.nrows(); i++) {
      if (weights[i] <= 0) {
        hasNegativeWeights = true;
        break;
      }
    }
    // Check if any element in values is less than 0
    bool hasNegativeValues = false;
    for (int i = 0; i < x.nrows(); i++) {
      if (values[i] < 0) {
        hasNegativeValues = true;
        break;
      }
    }

    // Checking x argument
    if (hasNegativeWeights || hasNegativeValues) {
      stop("The argument x should be a data frame with two variables v and w, with only positive values");
    }
  } else {
    stop("The argument x should be a data frame with two variables v and w");
  }

  // Checking W argument
  if (W <= 0) {
    stop("The argument W should be a positive integer, please check!");
  }

  int num_items = x.nrows();
  //create a matrix table to store the maximum values
  NumericMatrix x_mat(num_items + 1, W + 1);
  for (int i = 0; i <= num_items; i++) {
    for (int j = 0; j <= W; j++) {
      if (weights[i - 1] <= j) {
        x_mat(i, j) = std::max(x_mat(i - 1, j), x_mat(i - 1, j - weights[i - 1]) + values[i - 1]);
      } else {
        x_mat(i, j) = x_mat(i - 1, j);
      }
    }
  }

  // trace back to find the best elements
  NumericVector best_elements(num_items);
  int i = num_items;
  int j = W;

  while (i > 0 && j > 0) {
    if (x_mat(i - 1, j) != x_mat(i, j)) {
      best_elements[i] = 1;
      j = j - weights[i - 1];
    }
    i--;
  }

  double value = round(x_mat(num_items, W));

  // Find indices of selected elements
  IntegerVector elements;
  for (int i = 0; i < num_items; i++) {
    if (best_elements[i] == 1) {
      elements.push_back(i);
    }
  }
  return List::create(Named("value") = value, Named("elements") = elements);
}
')
