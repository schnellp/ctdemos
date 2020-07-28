generate.data <- function() {
  set.seed(0)
  
  n.per.arm <- 20
  N <- n.per.arm * 2
  
  regimen <- rep(c("paclitaxel", "docetaxel"), times = n.per.arm)[sample(N)]
  
  age <- round(rbeta(N, 5, 2) * (90 - 40) + 40)
  
  bmi <- round(rbeta(N, 5, 5) * (40 - 15) + 15)
  
  theta <- rnorm(N, mean = ifelse(regimen == "paclitaxel", 2, 3.2) + age / 100, sd = sqrt(10))
  
  follow.up <- pmax(1, pmin(round(rexp(N, rate = ifelse(regimen == "paclitaxel", 1 / 50, 1 / 20))), 5))
  
  y <- matrix(NA, nrow = N, ncol = 5)
  y.obs <- y
  y[, 1] <- rnorm(N, mean = (age / 10) ^ 2)
  y.obs[, 1] <- pmax(0, y[, 1])
  for (j in 2 : 5) {
    y[, j] <- y[, j - 1] + theta + rnorm(N)
    available <- as.logical(rbinom(N, 1, prob = pmax(0.7, age / 90)))
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
  
  # t.test((y.obs[, 5] - y.obs[, 1]) ~ regimen, var.equal = FALSE)
  # t.test((y[, 5] - y[, 1]) ~ regimen, var.equal = FALSE)
  
  data
}