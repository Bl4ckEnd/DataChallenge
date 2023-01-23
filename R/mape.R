mape <- function(actual, predicted) {
  mean(abs((actual-predicted)/actual))
}
