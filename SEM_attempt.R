## This script was created by Thomas Kettig as an exploration of what the Queen Elizabeth II
## pre-aspiration data looks like when modelled using lmers and SEMs.
## July 6, 2021

library(lavaan)
library(lme4)
library(semPlot)
library(tidyverse)

## Read in data

aggregated <- read.csv("aggregated_data.csv", stringsAsFactors = F, na.strings = c("", "NA"))

####### PREPARING DATA FOR MODELS ########

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

## Get a dataframe without outlier year, in case we wanna test that

no_1964 <- aggregated %>% filter(Year!=1964)

## Get the coding in place so lmers can be run

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
## I also have a script with some Bayesian versions in brms

model <- glmer(pre ~ 
                 + cr 
                # + br  
                # + Year_scaled 
                 + (1 | Word), data=aggregated, family="binomial")
summary(model)
# Shows positive correlation for both cr and br, negative for year

model <- glmer(pre ~ cr*br +  Year_scaled + (1 | Word), data=aggregated, family="binomial")
summary(model)
# There's also a significant interaction here


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


## Now let's try SEM on just subsets of footing

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


