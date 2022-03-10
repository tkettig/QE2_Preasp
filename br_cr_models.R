## Modelling the data
library(performance)
library(lme4)
library(sjPlot)
library(brms)
library(rstanarm)
library(bayestestR)
library(lavaan)
library(semPlot)
library(car)
options(mc.cores=parallel::detectCores())

setwd("/Users/Thomas/Documents/Preasp_Misa/R_scripts/")
source("data_cleaning.R")


## read in data
aggregated <- read.csv("/Users/Thomas/Documents/Preasp_Misa/aggregated_data.csv", stringsAsFactors = F, na.strings = c("", "NA"))



####### PREPARING FOR MODELS ########

## See the token counts

summary(as.factor(aggregated$vowel))

## Filter out the one token of ɔɪ

aggregated <- aggregated %>% filter(vowel!="ɔɪ")

## Filter out the few that are initial footing
summary(as.factor(aggregated$foot))
aggregated <- aggregated %>% filter(foot!="initial")

## Scaling year
aggregated$Year <- as.numeric(aggregated$Year)
aggregated$Year_scaled <- scale(aggregated$Year, scale=T)

## Make foot into numeric
aggregated$foot <- as.factor(aggregated$foot)
aggregated$footNUM <- as.numeric(aggregated$foot)

## Get a dataframe without outlier year

no_1964 <- aggregated %>% filter(Year!=1964)

## Get the coding in place so lmers can be run - go back 

aggregated <- aggregated %>% ungroup()
aggregated$Year <- as.numeric(aggregated$Year)
aggregated$vowel <- as.factor(aggregated$vowel)
aggregated$coda_cons <- as.factor(aggregated$coda_cons)
aggregated$foot <- as.factor(aggregated$foot)
aggregated$pre <- as.factor(aggregated$pre)
aggregated$pre_amb <- as.factor(aggregated$pre_amb)
aggregated$br <- as.factor(aggregated$br)
aggregated$br_amb <- as.factor(aggregated$br_amb)
aggregated$cr <- as.factor(aggregated$cr)

## Create two subsets, one for each foot

medial <- aggregated %>% filter(foot=="medial")
final <-  aggregated %>% filter(foot=="final")


####### Linear mixed effects models (regular frequentist ones to start) #######

model <- glmer(pre ~ cr + br +  Year_scaled + (1 | Word), data=aggregated, family="binomial")
summary(model)
# Shows positive correlation for both cr and br, negative for year

model <- glmer(pre ~ cr*br +  Year_scaled + (1 | Word), data=aggregated, family="binomial")
summary(model)
# There's also a significant interaction here


##### Bayesian logistic mixed effects regression models #####

## Setting priors here 

# for all betas (coefficients) except for intercept, we expect median value to be between 0-3 stdvs of normal distribution
# this is a weakly informative prior

priors <- set_prior("normal(0,3)",class="b")


### Medial dataset ###

model_brms_medial <- brm(pre ~ br*cr + Year_scaled + (1 | Word), data=medial, 
                          family = bernoulli(),
                          chains = 4, 
                          iter = 6000,
                          warmup = 1000,
                          prior = priors
)  

summary(model_brms_medial)
plot(rope(model_brms_medial, ci=0.95))
pairs(model_brms_medial)

## extract posterior samples
post_samples_model_brms_medial = posterior_samples(model_brms_medial)

## proportion of samples for each parameter that are negative/positive in the observed direction
mean(post_samples_model_brms_medial$b_br1 > 0) # 1 = The probability of br positively predicting pre is 1 (100%)
mean(post_samples_model_brms_medial$b_cr1 > 0) # 0.5 = no effect
mean(post_samples_model_brms_medial$b_Year_scaled < 0) # 1 = The probability of year negatively predicting pre is 1 (100%)
mean(post_samples_model_brms_medial$`b_br1:cr1` > 0) # 0.63 = no effect


### Final dataset ###
model_bayes_final <- brm(pre ~ br*cr + Year_scaled + (1 | Word), data=final, 
                         family = bernoulli(),
                         chains = 4, 
                         iter = 6000,
                         warmup = 1000,
                         prior = priors,
                         control = list(adapt_delta = 0.99) # this last thing is because there were divergent transitions without, this helps
)  

summary(model_bayes_final)
plot(rope(model_bayes_final, ci=0.95)) # says possible multicollinearity between cr and br and between br:cr and cr
pairs(model_bayes_final) # not sure how to interpret this, though

## extract posterior samples
post_samples_model_bayes_final = posterior_samples(model_bayes_final)

## proportion of samples for each parameter that are negative/positive in the observed direction
mean(post_samples_model_bayes_final$b_br1 > 0) # .99985 = The probability of br positively predicting pre is .99985
mean(post_samples_model_bayes_final$b_cr1 > 0) # .99985 = The probability of cr positively predicting pre is .99985
mean(post_samples_model_bayes_final$b_Year_scaled < 0) # .819 = The probability of year negatively predicting pre is .819
mean(post_samples_model_bayes_final$`b_br1:cr1` < 0) # .96505 = The probability of having both br and cr negatively predicting pre is .96505

check_model(model_brms_medial)
model_performance(model_brms_medial)
# doesn't let me check model though.... so going to try same thing with rstanarm below... but rstanarm one doesn't seem much better...

model_rstanarm_medial <- stan_glmer(pre ~ br*cr + Year_scaled + (1 | Word), data=medial, 
                         family = "binomial",
                         chains = 4, 
                         iter = 6000,
                         warmup = 1000,
)  
summary(model_rstanarm_medial)
plot(rope(model_rstanarm_medial))
check_model(model_rstanarm_medial)



#### Bayes sandbox, ignore #####




model_bayes <- brm(pre ~ br*foot + cr*foot + Year_scaled + (1 | Word), data=aggregated, 
                   family = bernoulli(),
                   chains = 4, 
                   iter = 6000,
                   warmup = 1000,
                   prior = priors
)  

summary(model_bayes)
plot(rope(model_bayes))
#plot(rope(model_bayes,ci=0.95))
tab_model(model_bayes)


model_bayes_2 <- brm(pre ~ br*cr*foot + Year_scaled + (1 | Word), data=aggregated, 
                     family = bernoulli(),
                     chains = 4, 
                     iter = 6000,
                     warmup = 1000,
                     prior = priors
)  
summary(model_bayes_2)
plot(rope(model_bayes_2))
# In this model, reference level is no br, no cr, final footing.

# This next will be identical, except I'll relevel foot

aggregated$foot <- fct_relevel(aggregated$foot, "medial","final")
contrasts(aggregated$foot)

model_bayes_3 <- brm(pre ~ br*cr*foot + Year_scaled + (1 | Word), data=aggregated, 
                     family = bernoulli(),
                     chains = 4, 
                     iter = 6000,
                     warmup = 1000,
                     prior = priors
)  
summary(model_bayes_3)
plot(rope(model_bayes_3))

## Okay, this could be a better way! Now I want to try to contrast code *just* foot and see what happens...

contrasts(aggregated$foot) = contr.sum(2)

model_bayes_3 <- brm(pre ~ br*cr*foot + Year_scaled + (1 | Word), data=aggregated, 
                     family = bernoulli(),
                     chains = 4, 
                     iter = 6000,
                     warmup = 1000,
                     prior = priors
)  
summary(model_bayes_3)
plot(rope(model_bayes_3))










####### Trying out SEM ###########

aggregated$pre <- as.numeric(aggregated$pre)
aggregated$br <- as.numeric(aggregated$br)
aggregated$cr <- as.numeric(aggregated$cr)

final$pre <- as.numeric(final$pre)
final$br <- as.numeric(final$br)
final$cr <- as.numeric(final$cr)
final <- final %>% mutate(br_cr = br * cr)

medial$pre <- as.numeric(medial$pre)
medial$br <- as.numeric(medial$br)
medial$cr <- as.numeric(medial$cr)
medial <- medial %>% mutate(br_cr = br * cr)

## SEM Model ##

model_indirect_brcr = '

# structural model

  pre ~ br + cr
  br + cr ~ Year_scaled
  
'
semModel_brcr = lavaan::sem(model_indirect_brcr, data=aggregated)
summary(semModel_brcr,fit.measures = TRUE, rsquare=TRUE)

semPaths(semModel_brcr, 
         what="par", 
         edge.label.cex = 1.1,
         nCharNodes=0,
         intercepts = F, 
         residuals = F, 
         fade = F, 
         layoutSplit = T, 
         measurementLayout = 'circle', 
         sizeMan=8)

## Okay, so a few interesting things: this is now giving me the correct direction of effects as I'd expected: she gets creakier and less breathy over the years
## And only breathiness is significantly positively correlated with preaspiration.
##
## I wonder how different this is, though, from just reporting on two glmers (or Bayesian equivalents):
## br ~ Year + (1 | Word)
## pre ~ br + (1 | Word)
## In this way, I could keep word in as a random intercept? But I get an error when I try the br ~ Year model as a regular glmer


## Now, I'm nearly certain this is not the intended use of making latent variables from manifest ones, but just to see what happens if 
## I try to model the interaction of footing with breathiness and creakiness this way...
## Breathiness+foot and creakiness+foot as intermediate between year and preaspiration.

## Hard-coding the interaction of br+foot and cr+foot since SEM syntax here can't seem to handle the * notation
aggregated$br_foot <- ifelse(aggregated$br == 1 & aggregated$foot == "medial", 1, 0)
aggregated$cr_foot <- ifelse(aggregated$cr == 1 & aggregated$foot == "medial", 1, 0)

model_indirect_brcrfoot = '

# measurement model

  brfoot =~ br + footNUM + br_foot
  crfoot =~ cr + footNUM + cr_foot
  
# structural model

  pre ~ brfoot + crfoot
  brfoot + crfoot ~ Year_scaled
  
'
semModel_brcrfoot = lavaan::sem(model_indirect_brcrfoot, data=aggregated)
## this is giving me error messages though
summary(semModel_brcrfoot,fit.measures = TRUE, rsquare=TRUE)


## Plotting graphical model of SEM ##

## Add labels of nodes

lbls <- c("Br","Foot","Year","Preasp","Glot","Br\nFoot","Glot\nFoot")

## Gonna try to add in the p-values
## Extract the parameters from the sem model and selecting the interactions relevant for the semPaths (here, I need 8 estimates and p-values)

table2<-parameterEstimates(semModel_brcrfoot,standardized=TRUE)  %>%  head(10)

## Turning the chosen parameters into text
b<-gettextf('%.2f, p=%.2f', table2$est, digits=table2$pvalue)

## Drawing the plots

## First, just the path

semPaths(semModel_brcrfoot, 
         what="path", 
         nCharNodes=0,
         intercepts = F, 
         residuals = F, 
         fade = F, 
         layoutSplit = T, 
         measurementLayout = 'circle', 
         sizeMan=8,
)

#nodeLabels=lbls
## Now, path plus intercept estimates and p-values

semPaths(semModel_brcrfoot, 
         what="par", 
         edge.label.cex = 1.1, 
         edgeLabels = b,
         nCharNodes=0,
         intercepts = F, 
         residuals = F, 
         fade = F, 
         layoutSplit = T, 
         measurementLayout = 'circle', 
         sizeMan=8,
)
#nodeLabels=lbls






###### Now let's try SEM on just subsets of footing

model_indirect_brcr_final = '

# structural model

  pre ~ br + cr + br_cr
  br + cr + br_cr ~ Year_scaled
  
'
semModel_brcr_final = lavaan::sem(model_indirect_brcr_final, data=final)
summary(semModel_brcr_final,fit.measures = TRUE, rsquare=TRUE)



model_indirect_brcr_medial = '

# structural model

  pre ~ br + cr + br_cr
  br + cr + br_cr ~ Year_scaled
  
'
semModel_brcr_medial = lavaan::sem(model_indirect_brcr_medial, data=medial)
summary(semModel_brcr_medial,fit.measures = TRUE, rsquare=TRUE)



x <- semPlotModel(semModel_brcrfoot)
x


t <- summary(model)
t <- with(aggregated, table(foot))
t

