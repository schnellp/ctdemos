generate.data.2 <- function() {
  set.seed(0)
  
  n.per.arm <- 30
  N <- n.per.arm * 2
  
  assignment <- rep(c("Treatment", "Control"), times = n.per.arm)[sample(N)]
  exposure <- assignment
  exposure[exposure == "Treatment"][as.logical(rbinom(n.per.arm, 1, 0.2))] <- "Control"
  assignment <- factor(assignment, levels = c("Control", "Treatment"))
  exposure <- factor(exposure, levels = c("Control", "Treatment"))
  
  outcome.binary <- rbinom(N, 1,
                           prob = 0.5 + 0.3 * (assignment == "treatment"))
  outcome.binary <- as.factor(ifelse(outcome.binary == 1, "Yes", "No"))
  
  outcome.continuous <- rnorm(N,
                              mean = -0.2 * (assignment == "treatment") -
                                0.5 * (exposure == "treatment"))
  
  outcome.tte.event <- rgamma(N, shape = 3,
                              rate = (1 + 1 * (exposure == "treatment")) / 10)
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