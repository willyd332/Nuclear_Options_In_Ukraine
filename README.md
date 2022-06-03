# Nuclear_Options_In_Ukraine


## This Repo

`Ukraine_Data_Factory.R` is the script used to create artificial data for pre-analysis

`Ukraine_Pre_Analysis.R` is the scripts used to run the regression



### (Hypothesis 1) *Decision makers who are briefed with explicit nuclear rhetoric are less likely to support a direct military response.*
In 1960, Glenn laid out his theory of the nuclear Stability-Instability Paradox. Rauchhaus tested Glenn’s ideas in 2009 using historical military data. He found that “the probability of major war between two states is indeed found to decrease when both states possess nuclear weapons.” Rauchhaus’s conclusions support the Stability half of the Stability-Instability Paradox. I expect that decision makers will continue this theoretical trend, falling into historical patterns of nuclear conflict when nuclear rhetoric is explicit.

### (Hypothesis 2) *Decision makers who are briefed with explicit nuclear rhetoric are more likely to support stronger indirect responses.*
Despite the decreased likelihood of direct warfare between nuclear powers, Rauchhaus found that “crisis initiation and limited uses of force” were made more likely by the possession of nuclear weapons by opposing sides. His conclusions support the Instability half of the Stability-Instability Paradox. If decision makers briefed with nuclear rhetoric stay true to Snyder’s theory, then they will continue this trend. Overall, I expect to see Synder’s paradox represented in the responses of decision makers: a desire to avoid direct nuclear conflict while simultaneously being provoked to stronger indirect action by the threat of nuclear conflict.

### Measurement & Treatment
Using an online survey experiment, I will ask respondents to adopt the role of the US president. After receiving a short vignette about the Ukraine crisis they will rate potential US responses to the conflict. The control group’s vignette will have no mention of Russia’s nuclear capabilities, while the treatment group’s vignette will have explicit mention of Russia’s nuclear capabilities. Assignment will be random. Respondents will answer five outcome questions (see Appendix A). The first four questions will suggest four possible US responses to Russia’s invasion of Ukraine: direct military response, indirect military aid, economic sanctions, or political condemnation. The respondents will decide the degree to which the US should respond in that way, measured on a scale of 0 to 5, with 5 being the most extreme and 0 being no such response at all. Finally, the respondents will be asked to decide the degree to which the US should respond in general, not tied to any specific response, using the same 0 to 5  scale. Additionally, respondents will undergo preliminary surveys to measure demographics and several thermometer tests to capture existing opinions on Russia and the use of military force in general.

### Analysis
To test this hypothesis I will use a regression analysis to find a relationship between the nuclear rhetoric used in the treatment vignette and subsequent proposed US responses. In this analysis, I will control for the respondent’s opinion on Russia and their opinion on the use of military force in general. The five survey questions will be used to create  5 variables: direct military response, indirect military aid, economic sanctions, political condemnation, and general degree of response, measured by regression coefficients  β, δ, ψ, λ, and  κ respectively. Under my Hypothesis 1, I expect the β and e κ coefficients to be negative.  Under Hypothesis 2, I expect δ, ψ, and λ coefficients to all be positive. These results would support the theory that decision makers in nuclear conflicts, while more cautious in direct and overall response, are less cautious and more aggressive when considering intermediate and indirect responses.
