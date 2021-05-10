# 1) Reading the data and scaling it
data <- read.csv("binary.csv", header = T)
X <- scale(data[, -1])
data <- as.data.frame(X)

# 2) SSD function
ssd <- function(x, y) {
  ret <- sum((x - y)^2)
  return(ret)
}

# 3) Creating a 4*4 grid for the SOM
crt_gr <- function(x) {
  ret <- matrix(data = rnorm(x * 3), nrow = x, ncol = 3)
  return(ret)
}

set.seed(222)
grid <- crt_gr(16)