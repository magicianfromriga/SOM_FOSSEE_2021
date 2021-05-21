# 1) Reading the data and scaling it
data <- read.csv("binary.csv", header = T)
X <- scale(data[, -1])
data <- data.table(X)

# 2) SSD function
ssd <- function(x, y) {
  ret <- sum((x - y)^2)
  return(ret)
}

# 3) Creating a 4*4 grid for the SOM
crt_gr <- function(n,p) {
  ret <- matrix(data = rnorm(n * p), nrow = n, ncol = p)
  return(as.data.table(ret))
}

set.seed(222)
grid <- crt_gr(16,3)

