library(dataverse)
library(tidyverse)
library(labelled)
library(modelsummary)
library(performance)
whan_data <- get_dataframe_by_name(
  filename = "final WP replication dataset.tab",
  dataset = "10.7910/DVN/JGE9OF",
  original=TRUE,
  .f = haven::read_dta,
  server = "dataverse.harvard.edu")
whan_data$warnumber<-factor(whan_data$warnumber)

# first model
model1 <- lm(lnnoncom ~ ghdumr + polity + raceorrel + apc + waraims2 + duration + finalprop  + lnnewpop,
  data = whan_data
)
# second model
model2 <- lm(lnnoncom ~ brat + polity  + raceorrel + apc + waraims2 + duration + finalprop + lnnewpop,
             data = whan_data)
#third model
model3 <- lm(
  lnnoncom ~ ghdumr + demtreat + demdum + raceorrel + apc + waraims2 + duration + finalprop + lnnewpop,
  data = whan_data
)
#fourth model
model4 <- lm(
  lnnoncom ~ ghdumr + demdum + apcdem +  raceorrel + apcdum + waraims2 + duration + finalprop  + lnnewpop,
  data = whan_data
)


#Replicating the main table (more or less)-----

# A mapping of coefficient names
map<-c(
  lnnoncom = "Non-Combatant Deaths (logged)",
# International Law: 
  ghdumr  = "Treaty Status",
  brat = "Mutual Treaty Ratification",
  demtreat = "Treaty * Regime Type",
# Alternative Hypotheses
  polity = "Regime Type",
  demdum = "Regime Type",
# Strategic Hypotheses
  apc = "Attrition or Counterinsurgency Strategy",
  apcdum = "Attrition or Counterinsurgency Strategy",
  raceorrel = 'Racial/Religious Conflict',
  waraims2 = "War Aims",
  duration = "War Duration",
# controls
  finalprop = "Relative Capabilities",
  lnnewpop = "Adversary Population Size (logged)",
  `(Intercept)` = "Constant"
)

model_list <- list("Model 1<br>(Baseline)" = model1, 
                   "Model 2<br>(Mutual Ratification)" = model2, 
                   "Model 3<br>(Regime Type * Treaty)" = model3, 
                   "Model 4<br>(Regime Type * Strategy)" = model4)

## Table----
# 1. Cluster SEs on War number
# 2. Use Bootstrapped standard errors

modelsummary(
  model_list,
  shape = term ~ statistic,
  statistic = c("std.error", "({p.value})"),
  coef_map = map,
  gof_map = c("nobs", "r.squared"),
  vcov = "bootstrap",
  R = 1000,
  cluster = ~ warnumber
  
  
)

#Predictions and marginal effects------

library(marginaleffects)

# average marginal effects:
avg_comparisons(model1, type='response', vcov=~warnumber)


# plotting predictions for geneva vs. non-geneva
plot_predictions(model1, condition=list('ghdumr'=c(0,1)),
                 type='response'
) +
  theme_bw()


# plotting predictions as function of duration
plot_predictions(model1,  condition=list('duration', 'ghdumr'),
                 type='response',
                 rug=TRUE
                 
) +
  theme_bw()







# Prediction checks ----
# Note the posterior predictive check in particular: 
check_model(model1)


# Zero inflation ----
# The authors argue that this isn't appropriate because the observation periods
# are different and the process of civilian killing varies across wars. Still:
# this looks *a lot* like zero inflation
plot(density(whan_data$lnnoncom))



# converting the logged civilian deaths back to counts. 
whan_data$counts <- round(exp(whan_data$lnnoncom)-1)

whan_data|>
  mutate(zeros = counts ==0 )|>
  select(zeros, ghdumr , 
           polity ,
           raceorrel ,
           apc ,
           waraims2  ,
           finalprop  , 
           lnnewpop )|>
  cor()


whan_data|>
  group_by(waraims2)|>
  summarise(prop0s  = mean(counts==0))


require(glmmTMB)
zero_inflated_model <- glmmTMB(counts ~ 
                   ghdumr + 
                   polity + 
                   raceorrel + 
                   apc + 
                   waraims2  + 
                   finalprop  + 
                   lnnewpop +
                   offset(log(duration)), 
                   ziformula = ~  waraims2 
                   ,  
                 data=whan_data, 
                 family=nbinom1(link='log'))
# in this model, the effect of signing the geneva conventions is negative and 
# significant.
summary(zero_inflated_model)



check_model(zero_inflated_model)

