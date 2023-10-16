#include <Rcpp.h>
using namespace Rcpp;
//' Dynamic search using Rcpp to solve the knapsack problem.
//' @references Reference page link <https://en.wikipedia.org/wiki/Knapsack problem#0.2F1 knapsack problem>
//' @description Dynamic search return the same results as brute force algorithm, but it should scale much better with O(W n) runing speed.
//' @param x a data frame which contains two variables v and w, stands for each items value and weight respectively.
//' @param W a postive integer which stands for the knapsack size
//' @returns the maximum knapsack value and which elements.
//' @examples
//' RNGversion(min(as.character(getRversion()),"3.5.3"))
//' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
//' n <- 2000
//' knapsack_objects <-
//' data.frame(
//' w=sample(1:4000, size = n, replace = TRUE),
//' v=runif(n = n, 0, 10000)
//' )
//' dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
//' @import Rcpp
//'  @export
//' [[Rcpp::export]]
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
// Rcpp Module
RCPP_MODULE(knapsack_module) {
  function("dynamic_knapsack_cpp", &dynamic_knapsack_cpp);
}

/*** R
# To load this module, use the sourceCpp function in R:
# sourceCpp("your_source_file.cpp")

# Example usage:
# x <- data.frame(w = c(2, 3, 4, 5), v = c(3, 4, 5, 6))
# W <- 5
# result <- dynamic_knapsack_cpp(x, W)
# print(result)
*/

