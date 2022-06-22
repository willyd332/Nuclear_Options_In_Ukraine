# Power Calculations
# https://egap.org/resource/10-things-to-know-about-statistical-power/

# KEY ASSUMPTIONS
COST_PER_RESPONSE = 0.80 
  # Based on GA minimum wage for a 5 minute survey + 0.2 cents just for fun
ALPHA = 0.01
TREATMENT_EFFECT_SIZE = 0.3
TARGET_POWER = 0.8
AVERAGE_OUTCOME = 2.5
STANDARD_DEVIATION = 1.47

# helpers
make_even <- function(x){
  if(x %% 2 != 0){
    x <- x + 1
  }
  return(x)
}

# RUNNING SIMULATED EXPERIMENTS
# possible.ns <- seq(from=100, to=2000, by=100) # The sample sizes we'll be considering
possible.ns <- seq(from=100, to=2000, by=100) * 0.66
possible.ns <- modify(possible.ns, make_even)
possible.ns
stopifnot(all( (possible.ns %% 2)==0 )) ## require even number of experimental pool
powers <- rep(NA, length(possible.ns)) # Empty object to collect simulation estimates 
alpha <- ALPHA # Standard significance level 
sims <- 500 # Number of simulations to conduct for each N 
#### Outer loop to vary the number of subjects #### 
for (j in 1:length(possible.ns)){
  N <- possible.ns[j] # Pick the jth value for N 
  Y0 <- rnorm(n=N, mean=60, sd=20) # control potential outcome 
  tau <- 5 # Hypothesize treatment effect 
  Y1 <- Y0 + tau # treatment potential outcome                                   
  significant.experiments <- rep(NA, sims) # Empty object to count significant experiments 
  
  #### Inner loop to conduct experiments "sims" times over for each N #### 
  Y0 <- rnorm(n=N, mean=AVERAGE_OUTCOME, sd=STANDARD_DEVIATION) # control potential outcome 
  tau <- TREATMENT_EFFECT_SIZE # Hypothesize treatment effect 
  Y1 <- Y0 + tau # treatment potential outcome 
    for (i in 1:sims){ 
      ## Z.sim <- rbinom(n=N, size=1, prob=.5) # Do a random assignment  by coin flip
      Z.sim <- sample(rep(c(0,1),N/2)) ## Do a random assignment ensuring equal sized groups
      Y.sim <- Y1*Z.sim + Y0*(1-Z.sim) # Reveal outcomes according to assignment 
      fit.sim <- lm(Y.sim ~ Z.sim) # Do analysis (Simple regression) 
      p.value <- summary(fit.sim)$coefficients[2,4] # Extract p-values 
      significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
    }
  powers[j] <- mean(significant.experiments) # store average success rate (power) for each N 
} 

powers_df = data.frame(
  sample_size = possible.ns,
  power = powers,
  cost = possible.ns * COST_PER_RESPONSE
  )

powers_df


# Next steps are to refine the simulation so that it works with multiple treatments
# Or you could just do what you were doing and assume 0.66 treatment size
# 
# Then you really need to refine your assumptions more...
#   1. Look for effect sizes from similar framing experiments to cite
#   2. Look ar IR public opinion studies to try to estimate standard deviation 
