# Power Calculations
# https://egap.org/resource/10-things-to-know-about-statistical-power/


# Assumptions
COST_PER_RESPONSE = 1
# How statistically significant must the results be?
BIG_ALPHA = 0.05
SMALL_ALPHA = 0.01
# How large am I assuming the treatment effect to be?
TREATMENT_EFFECT_SIZE = 0.3
# How often will the experiment return statistically significant results?
TARGET_POWER = 0.8
AVERAGE_OUTCOME_CONTROL = 2.12
AVERAGE_OUTCOME_TREATED = 2.72
STANDARD_DEVIATION = 1.47
SAMPLE_SIZE = 1000


power_calculator <- function(mu_t, mu_c, sigma, alpha=0.05, N){ 
  lowertail <- (abs(mu_t - mu_c)*sqrt(N))/(2*sigma) 
  uppertail <- -1*lowertail 
  beta <- pnorm(lowertail- qnorm(1-alpha/2), lower.tail=TRUE) + 1- pnorm(uppertail- qnorm(1-alpha/2), lower.tail=FALSE) 
  return(beta) 
} 

power_calculator(AVERAGE_OUTCOME_TREATED, AVERAGE_OUTCOME_CONTROL, STANDARD_DEVIATION, alpha=SMALL_ALPHA, SAMPLE_SIZE)
