generate.data.2 <- function() {
  set.seed(0)
  
  n.per.arm <- 30
  N <- n.per.arm * 2
  
  assignment <- rep(c("Treatment", "Control"), times = n.per.arm)[sample(N)]
  assignment <- factor(assignment, levels = c("Control", "Treatment"))
  
  outcome.binary <- rbinom(N, 1,
                           prob = 0.5 + 0.3 * (assignment == "Treatment"))
  outcome.binary <- as.factor(ifelse(outcome.binary == 1, "Yes", "No"))

  exposure <- assignment
  assigned <- assignment == "Treatment"
  exposure[assigned][
    as.logical(rbinom(n.per.arm, 1,0.1 + 0.3 * (outcome.binary[assigned] == "Yes")))
    ] <- "Control"
  exposure <- factor(exposure, levels = c("Control", "Treatment"))
  
  
  outcome.continuous <- rnorm(N,
                              mean = 1 * (assignment == "Treatment") +
                                1 * (exposure == "Treatment"))
  
  outcome.tte.event <- rgamma(N, shape = 3,
                              rate = ifelse(exposure == "Treatment", 1 / 40,
                                            ifelse(assignment == "Treatment", 1 / 1,
                                                   1 / 30)))
  outcome.tte.censor <- 60
  outcome.tte.observation <- pmin(outcome.tte.event, outcome.tte.censor)
  
  data <- data.frame(ID = as.factor(1 : N),
                     Assignment = assignment,
                     Exposure = exposure,
                     Analysis_Group = NA,
                     Adverse_Event = outcome.binary,
                     Pain_Improvement = round(outcome.continuous, 1),
                     Treatment_Length = round(outcome.tte.observation, 0))
  
  data
}