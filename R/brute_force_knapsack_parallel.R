#' brute-force search is a algorithm which is guaranteed to give a correct answer in all situations for the knapsack problem.
#' @references Reference page link <https://dataxujing.github.io/R_oop/RC.html#section-5.5.1>
#' @description brute_force search is going through all possible alternatives and return the maximum value found.
#' @param x a data frame which contains two variables v and w, stands for each items value and weight respectively.
#' @param W a postive integer which stands for the knapsack size
#' @param parallel to determine if using parallel to executue
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
#' brute_force_knapsack_parallel(x = knapsack_objects[1:8,], W = 3500)
#' @import parallel
#' @import profvis
#' @import utils
#' @export brute_force_knapsack_parallel



brute_force_knapsack_parallel <- function(x, W, parallel=FALSE)
{
  stopifnot(is.data.frame(x),W >= 0)

  if(parallel == FALSE)
  {
    object <- NULL
    weight <- NULL
    value <- NULL
    elements <- NULL

    for(i in 1:length(x$w))
    {
      object <- c(object,combn(1:length(x$w), i,paste,collapse = ","))
      weight <- c(weight,combn(x$w,i,sum))
      value <- c(value,combn(x$v,i,sum))
      total <- data.frame(object=object, weight=weight, value=value)
    }
  }

  else
  {
    #library(parallel)
    object <- NULL
    weight <- NULL
    value <- NULL
    elements <- NULL

    numofCores <- detectCores()-2
    cl <- makeCluster(numofCores)
    clusterExport(cl, c('x'), envir = environment())
    clusterEvalQ(cl , {require(parallel)})

    object <- parLapply(cl, 1:length(x$w), function(t) {

      combn(1:length(x$w),t,paste,collapse = ",")

    })
    weight <- parLapply(cl, 1:length(x$w), function(t) {
      combn(x$w,t,sum)
    })
    value  <- parLapply(cl, 1:length(x$v), function(t) {
      combn(x$v,t,sum)
    })
    total  <- data.frame(object=unlist(object), weight=unlist(weight), value=unlist(value))
    stopCluster(cl)
  }

  optimal_weights <- which(total$weight<=W)
  max_value <- max(total$value[optimal_weights])

  index <- which(total$value == max_value)
  y <- as.character(total$object[index])

  elements <- as.numeric(unlist(strsplit(y,",")))

  knapsack <- list(value = round(max_value),elements= elements)
  return(knapsack)
}


p<-profvis::profvis({
  interval=0.01

  suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
  n <- 2000
  knapsack_objects <-data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )



  brute_force_knapsack_parallel(x = knapsack_objects[1:16,], W = 3500)

})


print(p)


