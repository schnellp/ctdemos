generate.data <- function() {
  set.seed(0)
  
  n.per.arm <- 30
  N <- n.per.arm * 2
  
  regimen <- rep(c("paclitaxel", "docetaxel"), times = n.per.arm)[sample(N)]
  
  age <- round(rbeta(N, 5, 2) * (90 - 40) + 40)
  
  bmi <- round(rbeta(N, 5, 5) * (40 - 15) + 15)
  
  y <- matrix(NA, nrow = N, ncol = 5)
  y.obs <- y
  y[, 1] <- rnorm(N, mean = (age / 10) ^ 2)
  y.obs[, 1] <- pmax(0, y[, 1])
  
  theta <- rnorm(N,
                 mean = ifelse(regimen == "paclitaxel",
                               2,
                               -1 + ifelse(age >= 80, 10, 2)),
                 sd = sqrt(10))
  
  follow.up <- pmax(1, pmin(round(rexp(N, rate = ifelse(age >= 80, 1 / 10, 1 / 100))), 5))
  
  for (j in 2 : 5) {
    y[, j] <- y[, j - 1] + theta + rnorm(N)
    available <- (j == 5) | as.logical(rbinom(N, 1, prob = 0.8))
    y.obs[available, j] <- pmax(0, y[available, j])
    y.obs[follow.up < j, j] <- NA
  }
  
  
  data <- data.frame(subject = as.factor(1 : N),
                     age = age,
                     bmi = bmi,
                     regimen = regimen,
                     baseline = round(y.obs[, 1]),
                     cycle.1 = round(y.obs[, 2]),
                     cycle.2 = round(y.obs[, 3]),
                     cycle.3 = round(y.obs[, 4]),
                     follow.up = round(y.obs[, 5]))
  
  data
}