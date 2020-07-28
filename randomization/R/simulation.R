SEED_OFFSET_SUBJECTS <- 1
SEED_OFFSET_STRATA <- 1000

generate.subject <- function() {
  site <- factor(sample(1 : 4,
                        size = 1,
                        prob = c(4, 3, 2, 1)),
                 levels = 1 : 4)
  
  sex <- factor(sample(c("female", "male"),
                       size = 1,
                       prob = c(30, 70)),
                levels = c("female", "male"))
  
  age <- round(30 + rbeta(1, 2, 5) * 70)
  age.binary <- factor(ifelse(age < 65, "<65", "65+"),
                       levels = c("<65", "65+"))
  
  race <- factor(sample(c("asian", "black", "white", "other/multiple"),
                        size = 1,
                        prob = c(5, 10, 80, 5)),
                 levels = c("asian", "black", "white", "other/multiple"))
  
  arthritis <- factor(sample(c("no", "yes"),
                             size = 1,
                             prob = c(65, 35)),
                      levels = c("no", "yes"))
  
  pasi <- round(10 + rbeta(1, 1, 4) * 40)
  pasi.binary <- factor(ifelse(pasi < 15, "<15", "15+"),
                        levels = c("<15", "15+"))
  
  hamd <- factor(sample(c("none", "mild", "moderate-to-severe"),
                        size = 1,
                        prob = c(75, 20, 5)),
                 levels = c("none", "mild", "moderate-to-severe"))
  
  return(data.frame(
    site,
    sex, age, age.binary, race,
    arthritis, pasi, pasi.binary, hamd
  ))
}

generate.data <- function(n) {
  
  data <- data.frame()
  
  for (i in 1 : n) {
    data <- rbind(data, generate.subject())
  }
  
  return(data)
}


generate.randomization.schedule.simple <- function(n.subs, odds) {
  n.arms <- length(odds)
  arm <- factor(sample(paste("Arm", 1 : n.arms), n.subs, replace = TRUE, prob = odds),
                levels = paste("Arm", 1 : n.arms))
  
  return(arm)
}

generate.randomization.schedule.block <- function(n.subs, odds) {
  n.arms <- length(odds)
  
  arm <- character(0)
  
  while (length(arm) < n.subs) {
    block <- paste("Arm", rep(1 : n.arms, times = odds))
    arm <- c(arm, sample(block,
                         size = min(length(block), n.subs - length(arm)),
                         replace = FALSE,
                         prob = NULL))
  }
  
  arm <- factor(arm, levels = paste("Arm", 1 : n.arms))
  
  return(arm)
}

construct.strata <- function(data, vars) {
  n.subs <- nrow(data)
  
  var.levels <- list()
  stratum.levels <- matrix("")
  stratum <- data.frame()
  for (var in vars) {
    var.levels[[var]] <- levels(data[[var]])
    stratum.levels <- matrix(outer(t(stratum.levels),
                                   paste0(var, var.levels[[var]]),
                                   FUN = paste, sep = ":"))
    stratum <- paste(stratum,
                     paste0(var, data[[var]]),
                     sep = ":")
  }
  
  stratum <- factor(stratum, levels = as.vector(stratum.levels))
  
  return(stratum)
}

generate.randomization.schedule <- function(seed,
                                            data, stratify.on,
                                            odds, type) {
  
  n.subs <- nrow(data)
  
  if (length(stratify.on) > 0) {
    stratum <- construct.strata(data, vars = stratify.on)
  } else {
    stratum <- factor(rep("all", n.subs), levels = "all")
  }
  
  data$arm <- factor(NA, levels = paste("Arm", 1 : length(odds)))
  
  
  
  n.subs <- nrow(data)
  
  if (type == "simple") {
    rand.fun <- generate.randomization.schedule.simple
  } else if (type == "block") {
    rand.fun <- generate.randomization.schedule.block
  }
  
  for (stratum.level in levels(stratum)) {
    set.seed(seed + SEED_OFFSET_STRATA + which(levels(stratum) == stratum.level))
    in.stratum <- which(stratum == stratum.level)
    data$arm[in.stratum] <- rand.fun(n.subs = length(in.stratum), odds = odds)
  }
  
  return(data$arm)
}

simulate <- function(seed, n.subs, n.arms,
                     stratify.on, random.type, odds) {
  set.seed(seed)
  
  data <- generate.data(n.subs)
  
  set.seed(seed + SEED_OFFSET_SUBJECTS)
  
  rand.sched <- generate.randomization.schedule(
    seed = seed,
    data = data,
    stratify.on = stratify.on,
    odds = odds,
    type = random.type
  )
  
  rand.sched <- factor(rand.sched, levels = paste("Arm", 1 : n.arms))
  
  data <- cbind(arm = rand.sched, data)
}