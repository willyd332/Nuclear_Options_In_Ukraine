# Power Calculations
# https://egap.org/resource/10-things-to-know-about-statistical-power/

# KEY ASSUMPTIONS
COST_PER_RESPONSE = 0.80 # Based on GA minimum wage for a 5 minute survey + 20 Cents
ALPHA = 0.01
TREATMENT_EFFECT_SIZE = 0.26
TARGET_POWER = 0.8
AVERAGE_OUTCOME = 1.84548
STANDARD_DEVIATION = 1.112738

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
stopifnot(all( (possible.ns %% 2)==0 )) # require even number of experimental pool
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



# Kertzer & Zeitoff (2017) FOR ASSUMPTIONS:
# ----------------------------------------------------------------------------------------------------
# A Bottom-Up Theory of Public Opinion about Foreign Policy
  # Kertzer & Zeitoff 2017
  # Replication Data: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/QSXDUN

# "Like caesar onto Gaul, we can crudely divide [FP Public Opinion literature]
# into three parts."
  # Almond-Lippmann consensus: 
    # Pessimistic view, public opinion on IR is ill-informed and ill-structured
    # Early/Mid 20th century
    # Foreign Policy Realists
  # The Optimists
    # FP Opinions does have structure and public reacts predictably and prudently
    # Public likes victory, success, consistency, multilateralism, and order
  # Top Down Approach
    # Public does have coherent opinions, but they are defined by foreign policy elites
    # Public is "rationally ignorant" due to the information asymmetry in foreign policy
      # "In the heat of the crisis in Ukraine in early March 2014, for example, only one in six 
      # Americans could correctly locate Ukraine on a map (Dropp, Kertzer, and Zeitzoff 2014)"
    # Driven from the top down along party lines (take cues from elites of the preferred party)

# Some complications to this triad
  # Are we sure that public opinion doesn't drive elites?
  # Elites may be more likely to change opinions based on voters than vice versa
  # There is often a disconnect between public and elites (Iraq War)

# The Experiments

# "To explore these mesofoundations of public opinion about foreign affairs, we designed five 
# survey experiments, fielded in three different studies. The first two experiments were embedded 
# in a survey fielded by Survey Sampling International (SSI) on a national sample of 1,035 
# registered voters in the summer of 2014.3  The third experiment was administered to 1,446 
# American adults on Amazon Mechanical Turk (MTurk) in the autumn of 2014. The fourth and fifth 
# experiments were embedded in a survey administered to 1,997 American adults on MTurk in the
# autumn of 2016. We describe each in turn."

  # They presented news article with quote from either Democratic or Republican elite
  # and asked about various opinions...

# STANDARD DEVIATION ASSUMPTION 
  # For the control group, the sd for military involvement was: [scale of 0-1]
    # 0.2671561 for Asia Military Involvement (Study 1) 
    # 0.3005846 for Terrorism Military Involvement (Study 1)
    # 0.2654485 for Asia Military Involvement (Study 2) 
    # 0.2795486 for Increased Naval Presence In Asia (Study 3)
    # Converted to my scale:
    mean_sd <- mean(c(0.2671561,0.3005846,0.2654485,0.2795486))
    # 0.2781844
    assumed_sd <- mean_sd * 4 # scale of 0-1 converted to 0-4
    assumed_sd # 1.112738

# MEAN ASSUMPTION 
  # For the control group, the mean for military involvement was: [scale of 0-1]
    # 0.4618014 for Asia Military Involvement (Study 1) 
    # 0.4853293 for Terrorism Military Involvement (Study 1)
    # 0.4042131 for Asia Military Involvement (Study 2) 
    # 0.4941358 for Increased Naval Presence In Asia (Study 3)
    # Converted to my scale:
    mean_mean <- mean(c(0.4618014,0.4853293,0.4042131,0.4941358))
    # 0.4613699
    assumed_mean <- mean_mean * 4 # scale of 0-1 converted to 0-4
    assumed_mean # 1.84548
    
# TREATMENT EFFECT ASSUMPTION
  # Taking the absolute effect of news story along party lines on opinion
  # I'll assume that if FP opinions change by reading something, they will
  # likely change by a consistent amount (ALL ABSOLUTE VALUE)
    # STUDY 1
    #               China - Terrorism
    # Group Endorse 0.070 - 0.054
    # Group Oppose  0.066 - 0.064
    #
    # STUDY 2
    #               China 
    # Group Endorse 0.043
    # Group Oppose  0.064
    #
    # STUDY 3
    #               Navy
    # Group Endorse -    
    # Group Oppose  0.094
    #
    mean_effect <- mean(c(0.070,0.054,0.066,0.064,0.043,0.094,0.064))
    # 0.065
    assumed_effect <- mean_effect * 4 # scale of 0-1 converted to 0-4
    assumed_effect # 0.26
    
    
    
    
    
    