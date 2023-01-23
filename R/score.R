rmse <- function(y, ychap) {
  z = sqrt(mean((y-ychap)^2))
  z
}