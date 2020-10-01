simulate <- function(seed,
                     n.trt, n.ctl,
                     delta.trt, delta.ctl,
                     sd,
                     rho,
                     design, alpha) {
  set.seed(seed)
  
  N <- 2 * (n.trt + n.ctl)
  
  var.tot <- sd ^ 2
  var.between <- var.tot * rho
  var.within <- var.tot - var.between
  
  exposure <- c(
    rep("Treatment", n.trt),
    rep("Control", n.ctl),
    rep("Control", n.trt),
    rep("Treatment", n.ctl)
  )
  exposure <- rep(exposure, each = 2)
  
  period <- rep(c("First", "Second"), each = n.trt + n.ctl)
  period <- rep(period, each = 2)
  
  time <- rep(c("Start", "End"), times = 2 * (n.trt + n.ctl))
  
  theta <- rep(rep(rnorm(n.trt + n.ctl, mean = 5, sd = sqrt(var.between)), each = 2), 2)
  
  epsilon <- rnorm(2 * N, mean = 0, sd = sqrt(var.within))
  
  delta.map <- c("Treatment" = delta.trt, "Control" = delta.ctl)
  measurement <- theta + epsilon + delta.map[exposure] * (time == "End")
  
  subject <- rep(rep(1 : (n.trt + n.ctl), each = 2), times = 2)
  
  data <- data.frame(subject, exposure, period, time, measurement)
  
  if (design == "diff") {
    data.analysis <- data[data$period == "First" & data$time == "End", ]
    test <- t.test(measurement ~ exposure, data = data.analysis)
    est <- diff(test$estimate)
  } else if (design == "change") {
    data.analysis <- data[data$period == "First", ]
    start.index <- which(data.analysis$time == "Start")
    end.index <- which(data.analysis$time == "End")
    if (!all(data.analysis$subject[start.index] == data.analysis$subject[end.index])) {
      stop("Measurements out of order.")
    }
    outcome <- data.analysis$measurement[end.index] - data.analysis$measurement[start.index]
    group <- data.analysis$exposure[end.index]
    test <- t.test(outcome ~ group)
    est <- diff(test$estimate)
  } else if (design == "cross") {
    data.analysis <- data[data$time == "End", ]
    
    first.index <- which(data.analysis$period == "First")
    second.index <- which(data.analysis$period == "Second")
    if (!all(data.analysis$subject[first.index] == data.analysis$subject[second.index])) {
      stop("Measurements out of order.")
    }
    outcome <- (data.analysis$measurement[first.index] -
                  data.analysis$measurement[second.index]) *
      ifelse(data.analysis$exposure[first.index] == "Treatment", 1, -1)
    test <- t.test(outcome)
    est <- test$estimate
  }
  
  return(c("tstat" = unname(test$statistic),
           "pval" = unname(test$p.value),
           "est" = unname(est)))
}
