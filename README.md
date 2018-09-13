---
title: "TLC Data Analysis"
output: ''
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Ok so I need to transpose the data.  Use row.names = NULL forces numbering, so that will be used when we transpose it

Then grab the variables that we want.  We want the id, treatment, section 1, sections 1 through 4 and then demographics

Then we are renaming every variable. 

Clean the adult data set first then youth



## Testing data trying to find how the averages over time are the exact same
```{r}
library(foreign)
library(nnet)
library(ggplot2)
library(prettyR)
library(nlme)
library(prettyR)
library(descr)
library(Amelia)
library(mitools)
library(BaylorEdPsych)
library(openxlsx)
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(DescTools)
library(MissMech)
library(robustlmm)
library(jtools)
library(paran)
library(effsize)
library(multcomp)

#rm(list=ls())
#setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
#datPreAdult = read.csv("Target1EnhancedBaseAdult.csv", header = TRUE)
#datPostAdult = read.csv("Target1EnhancedPostAdult.csv", header = TRUE)


head(datPreAdult)
# subset the variables that you want
datPreAdult =datPreAdult[c(1, 3:4, 6,8,10, 12:13, 15:34, 36:45, 47:51, 53:59)]

head(datPostAdult)
datPostAdult = datPostAdult[c(1, 3:22,24:33, 35:39, 41:47)]
head(datPostAdult)

# Rename added variables otherwise everything else should be the same
colnames(datPreAdult)[colnames(datPreAdult) == "Added.V2..Thinking.of.Ways.to.Kill.Self"] = "Added"   

## Now merge everything
datAdult = merge(datPreAdult, datPostAdult, by = "Adult.ID", sort = TRUE)
head(datAdult)

### 357 is still around above

# Need to er
#datAdultTreat = read.csv("AdultTreatment.csv", header = TRUE)
#Now we need to merge the treatment variable before we transform into long format to avoid duplication.

head(datAdultTreat)

datAdult = merge(datAdult, datAdultTreat, by = "Adult.ID", all.x = TRUE, sort = TRUE)
head(datAdult)




datAdult = reshape(datAdult, varying = list(c("Desire.to.succeed.x", "Desire.to.succeed.y"), c("My.own.plan.to.stay.well.x", "My.own.plan.to.stay.well.y"), c("Goals.in.life.x", "Goals.in.life.y"), c("Believe.I.can.meet.personal.goals.x", "Believe.I.can.meet.personal.goals.y"), c("Purpose.in.life.x", "Purpose.in.life.y"), c("Fear.doesn.t.stop.me......x", "Fear.doesn.t.stop.me......y"), c("I.can.handle.my.life.x", "I.can.handle.my.life.y"), c("I.like.myself.x", "I.like.myself.y"), c("If.people.really.knew.me.......x", "If.people.really.knew.me.......y"), c("Who.I.want.to.become.x", "Who.I.want.to.become.y"), c("Something.good.will.happen.x", "Something.good.will.happen.y"), c("I.m.hopeful.about.future.x", "I.m.hopeful.about.future.y"), c("Continue.to.have.new.interests.x", "Continue.to.have.new.interests.y"), c("Coping.with.mental.illness.not.focus.of.life.x", "Coping.with.mental.illness.not.focus.of.life.y"), c("Symptoms.interfere.less.and.less.x", "Symptoms.interfere.less.and.less.y"), c("Symptoms.problem.for.shorter.periods.x", "Symptoms.problem.for.shorter.periods.y"), c("Know.when.to.ask.for.help.x", "Know.when.to.ask.for.help.y"), c("Willing.to.ask.for.help.x", "Willing.to.ask.for.help.y"), c("I.ask.for.help.when.I.need.it..x", "I.ask.for.help.when.I.need.it..y"), c("I.can.handle.stress..x", "I.can.handle.stress..y"), c("Better.off.if.I.were.gone.x", "Better.off.if.I.were.gone.y"), c("Happier.without.me.x", "Happier.without.me.y"), c("Death.would.be.a.relief.x", "Death.would.be.a.relief.y"), c("Wish.they.could.be.rid.of.me.x", "Wish.they.could.be.rid.of.me.y"), c("Make.things.worse.x", "Make.things.worse.y"), c("Feel.like.I.belong.x", "Feel.like.I.belong.y"), c("Have.many.caring.and.supportive.friends.x", "Have.many.caring.and.supportive.friends.y"), c("Feel.disconnected.x", "Feel.disconnected.y"), c("Feel.like.an.outsider.x", "Feel.like.an.outsider.y"), c("Close.to.other.people.x", "Close.to.other.people.y"), c("Unable.to.take.care.of.self.x", "Unable.to.take.care.of.self.y"), c("Not.recover.or.get.better.x", "Not.recover.or.get.better.y"), c("I.am.to.blame.x", "I.am.to.blame.y"), c("Unpredictable.x", "Unpredictable.y"), c("Dangerous.x", "Dangerous.y"), c("Wish.life.would.end..x", "Wish.life.would.end..y"), c("Life.not.worth.living.x", "Life.not.worth.living.y"), c("Life.so.bad..feel.like.giving.up..x", "Life.so.bad..feel.like.giving.up..y"), c("Better.for.everyone.if.I.were.to.die..x", "Better.for.everyone.if.I.were.to.die..y"), c("Added.x", "Added.y"), c("No.solution.to.my.problems.x", "No.solution.to.my.problems.y"), c("Believe.my.life.will.end.in.suicide..x", "Believe.my.life.will.end.in.suicide..y")), direction = "long", times = c(0,1))



colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "Treatment", "Time", "RAS1", "RAS2", "RAS3", "RAS4", "RAS5", "RAS6", "RAS7", "RAS8", "RAS9", "RAS10", "RAS11", "RAS12", "RAS13", "RAS14", "RAS15", "RAS16", "RAS17", "RAS18", "RAS19", "RAS20", "INQ1", "INQ2", "INQ3", "INQ4", "INQ5", "INQ6", "INQ7", "INQ8", "INQ9", "INQ10", "SSMI1", "SSMI2", "SSMI3", "SSMI4", "SSMI5", "SIS1", "SIS2", "SIS3", "SIS4", "SIS5", "SIS6", "SIS7")

# Drop last column id 
datAdult = data.frame(datAdult)
head(datAdult)
datAdult$NA. = NULL
head(datAdult)
dim(datAdult)
head(datAdult)



#Checking for issues with adult data set

summary(datAdult)

# One person age is 451 get rid of them
datAdult = subset(datAdult, Age < 450)
dim(datAdult)



# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == " B", "B", datAdult$Treatment)
compmeans(datAdult$RAS1, datAdult$Time)

describe.factor(datAdult$Treatment)

describe.factor(datAdult$Treatment)
datAdult$Treatment = ifelse(datAdult$Treatment == 8, 1, ifelse(datAdult$Treatment == 3, 2, ifelse(datAdult$Treatment == 6, 3, ifelse(datAdult$Treatment == "B", 2, datAdult$Treatment))))

describe.factor(datAdult$Treatment)

## If there are NA's for treatment then need to drop them
datAdult = subset(datAdult, Treatment == 1 | Treatment == 2 | Treatment == 3)
compmeans(datAdult$RAS1, datAdult$Time)

# Three items are reversed scored: f = 6, g = 7, j = 10
datAdult$INQ6 = ifelse(datAdult$INQ6 == 1, 5, ifelse(datAdult$INQ6 == 2,4, ifelse(datAdult$INQ6  == 3,3, ifelse(datAdult$INQ6  == 4,2, ifelse(datAdult$INQ6  == 5,1,datAdult$INQ6)))))

datAdult$INQ7= ifelse(datAdult$INQ7== 1, 5, ifelse(datAdult$INQ7== 2,4, ifelse(datAdult$INQ7 == 3,3, ifelse(datAdult$INQ7 == 4,2, ifelse(datAdult$INQ7 == 5,1,datAdult$INQ7)))))

datAdult$INQ10= ifelse(datAdult$INQ10== 1, 5, ifelse(datAdult$INQ10== 2,4, ifelse(datAdult$INQ10 == 3,3, ifelse(datAdult$INQ10 == 4,2, ifelse(datAdult$INQ10 == 5,1,datAdult$INQ10)))))


head(datAdult)

### 357 is still around above
###### Scores are different above but very similar

## Now long format so don't have to rename twice



# Rename everything, which you will rename for the youth data set as well 
# 





# Create pre data sets for psychometrics
datAdultPre = subset(datAdult, Time == 0)
datAdultPost = subset(datAdult, Time == 1)

head(datAdultPre)
RAS = datAdultPre[c(11:30)]
head(RAS)

head(datAdultPre)
INQ = datAdultPre[c(31:40)]
head(INQ)

SSMI = datAdultPre[c(41:45)]
head(SSMI)

SIS = datAdultPre[c(46:52)]
SIS

# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
RASSub1 = RAS[c(6:13, 20)]


# Subscale two q = 17, r= 18, s= 19
head(RAS)
RASSub2 = RAS[c(17:19)]
# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
head(RAS)
RASSub3 = RAS[c(1:5)]

# Subscale five: n = 14, o = 15, p = 16
head(RAS)
RASSub5 = RAS[c(14:16)]


#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5
INQSub1 = INQ[c(1:5)]

#Subscale 2 for INQ: f-j: 6-10
INQSub2 = INQ[c(6:10)]

#Subscale 1 for SIS: a-d: 1:4
SISSub1 = SIS[c(1:4)]

#Subscale 2 for SIS: e-g: 5:7
SISSub2 = SIS[c(5:7)]



# Creating sum scores for the data analysis that contains all data not just pre data
datAdultDemos = datAdult[c(1:10)]
head(datAdultDemos)

RASPrePost = datAdult[c(11:30)]
head(RAS)
INQPrePost = datAdult[c(31:40)]
head(INQ)
SSMIPrePost = datAdult[c(41:45)]
head(SSMI)
SISPrePost = datAdult[c(46:52)]
head(SIS)

RASTotalScore = rowSums(RASPrePost)
INQTotalScore = rowSums(INQPrePost)
SSMITotalScore = rowSums(SSMIPrePost)
SISTotalScore = rowSums(SISPrePost)

datAdultAnalysis = data.frame(datAdultDemos, RASTotalScore, INQTotalScore, SSMITotalScore, SISTotalScore)


#Need code gender, race, sexual orientation, edu, employment, RelationshipStatus as binary
#Gender: 2 = 1, 1 = 0
#Race: 7 = 0, all else 1
#Sex Orien: 3 = 0, all else 1
#Edu: 2 = 1, all else = 0; high school over lower for one
#Employment 1 = 1 else = 0; unemployed versus everyone else
#Relationship Status: 1,2,3,4 = 1 else = 0


datAdultAnalysis$Gender = ifelse(datAdultAnalysis$Gender == 2,1, 0)
datAdultAnalysis$Race = ifelse(datAdultAnalysis$Race == 7,0, 1)
datAdultAnalysis$SexualOrientation = ifelse(datAdultAnalysis$SexualOrientation == 3,0, 1)
datAdultAnalysis$Edu = ifelse(datAdultAnalysis$Edu == 2,1, 0)
datAdultAnalysis$Employment = ifelse(datAdultAnalysis$Employment == 1,1, 0)


datAdultAnalysis$RelationshipStatus = ifelse(datAdultAnalysis$RelationshipStatus <= 4, 1, 0)
describe.factor(datAdultAnalysis$RelationshipStatus)


# For the complete data set I need to drop SIS, because there is a ton of missing data
datAdultAnalysisComplete = datAdultAnalysis
datAdultAnalysisComplete$SISTotalScore = NULL
datAdultAnalysisComplete = na.omit(datAdultAnalysisComplete)

### Create data for t-tests
datAdultAnalysisCompletePre = subset(datAdultAnalysisComplete, Time == 0) 

head(datAdultAnalysisCompletePre)

datAdultAnalysisCompletePost = subset(datAdultAnalysisComplete, Time == 1) 
head(datAdultAnalysisCompletePost)
datAdultAnalysisCompletePost

datAdultAnalysisCompleteT.test = merge(datAdultAnalysisCompletePre, datAdultAnalysisCompletePost, by = "ID", all.x = TRUE)

datAdultAnalysisCompleteT.testComplete = na.omit(datAdultAnalysisCompleteT.test)
dim(datAdultAnalysisCompleteT.testComplete)

```

Get reliability measure
```{r}
# RAS overall

omegaRAS = omega(RAS)
summary(omegaRAS)

omegaRASSub1 =  omega(RASSub1)
summary(omegaRASSub1)


omegaRASSub2 =  omega(RASSub2)
summary(omegaRASSub2)


omegaRASSub3 =  omega(RASSub3)
summary(omegaRASSub3)


omegaRASSub5 =  omega(RASSub5)
summary(omegaRASSub5)

omegaINQ = omega(INQ)
summary(omegaINQ)

omegaINQSub1 = omega(INQSub1)
summary(omegaINQSub1)

omegaINQSub2 = omega(INQSub2)
summary(omegaINQSub2)

omegaSSMI = omega(SSMI)
summary(omegaSSMI)

omegaSIS = omega(SIS)
summary(omegaSIS)

omegaSISSub1 = omega(SISSub1)
summary(omegaSISSub1)

omegaSISSub2 = omega(SISSub2)
summary(omegaSISSub2)

```
Now try both versions of parrallel analysis
Only works with complete data

Use CFA instead of PCA, and using a more conservative approach to factor retention (parrallel can extract too many factors) See Glorfeld(1995): https://drive.google.com/file/d/1HehR1z1qY4GZkMy8YKqPqHA1coKGOdMB/view?usp=sharing   

RAS Parallel
```{r}
RASComplete = na.omit(RAS)
paran(x = RASComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
```
INQ Parallel 
```{r}
INQComplete = na.omit(INQ)
paran(x = INQComplete,centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
```
SSMI Paralell
```{r}
SSMIComplete = na.omit(SSMI)
paran(x = SSMIComplete,centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
```
SIS Parallel 
```{r}
SISComplete = na.omit(SIS)
paran(x = SISComplete,centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
```
Now MAP and VSS tests
```{r}
vss(RAS, n = 4)
vss(INQ, n = 4)
vss(SSMI, n = 4)
vss(SIS, n =4)
```
Now efa tests
```{r}
efaRAS <- efaUnrotate(RAS, nf=4, estimator="mlr", missing = "ML")
inspect(efaRAS, "std")

efaINQ <- efaUnrotate(INQ, nf=4, estimator="mlr", missing = "ML")
inspect(efaINQ, "std")

efaSSMI <- efaUnrotate(SSMI, nf=2, estimator="mlr", missing = "ML")
inspect(efaSSMI, "std")

efaSIS <- efaUnrotate(SIS, nf=3, estimator="mlr", missing = "ML")
inspect(efaSIS, "std")

```
Now generate CFA's for each of them

RAS for first five
# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20


# Subscale two q = 17, r= 18, s= 19

# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5


# Subscale five: n = 14, o = 15, p = 16


#Subscale 1 for INQ: a = 1, b = 2, c = 3, d = 4, e = 5


#Subscale 2 for INQ: f-j: 6-10


#Subscale 1 for SIS: a-d: 1:4


#Subscale 2 for SIS: e-g: 5:7


Weighted and diagnol least squares does not work with missing data
```{r}
modelRAS = 'RASL1 =~ RAS6 + RAS7 + RAS8 + RAS9 + RAS10 + RAS11 + RAS12 + RAS13 + RAS20
          RASL2 =~ RAS17 + RAS18 + RAS19
          RASL3 =~ RAS1 + RAS2 + RAS3 + RAS4 + RAS5
          RASL5 =~ RAS14 + RAS15 + RAS16
          '

fitRAS = cfa(modelRAS, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = RAS)
summary(fitRAS, fit.measures = TRUE)

modelRAS = 'RASL1 =~ RAS6 + RAS7 + RAS8 + RAS9 + RAS10 + RAS11 + RAS12 + RAS13 + RAS20
          RASL2 =~ RAS17 + RAS18 + RAS19
          RASL3 =~ RAS1 + RAS2 + RAS3 + RAS4 + RAS5
          RASL5 =~ RAS14 + RAS15 + RAS16
          '

modelINQ ='
          INQL1 =~ INQ1 + INQ2 + INQ3 + INQ4 + INQ5
          INQL2 =~ INQ6 + INQ7 + INQ8 + INQ9 + INQ10
          '
fitINQ = cfa(modelINQ, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = INQ)
summary(fitINQ, fit.measures = TRUE)


modelSSMI ='
           SSMI =~ SSMI1 + SSMI2 + SSMI3 + SSMI4 + SSMI5         
           '
fitSSMI = cfa(modelSSMI, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = SSMI)
summary(fitSSMI, fit.measures = TRUE)


modelSIS = '
           SIS =~ SIS1 + SIS2 + SIS3 + SIS4 + SIS5 +SIS6 
          '
fitSIS = cfa(modelSIS, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = SIS)
summary(fitSIS, fit.measures = TRUE)           

```
Get descirptives for non-imputed data
```{r}
datAdultAnalysisPre = subset(datAdultAnalysisComplete, Time == 0)
describe(datAdultAnalysisPre)

datAdultAnalysisPost = subset(datAdultAnalysisComplete, Time == 1)
describe(datAdultAnalysisPost)



```
Post means
```{r}
compmeans(datAdultAnalysisPost$RASTotalScore, datAdultAnalysisPost$Treatment)
compmeans(datAdultAnalysisPre$RASTotalScore, datAdultAnalysisPre$Treatment)

```
Pre means
```{r}

```


Now test for missing at random and just run imputed afterwareds
```{r}
write.csv(datAdultAnalysis, "datAdultAnalysis.csv", row.names = FALSE)
datAdultAnalysis = read.csv("datAdultAnalysis.csv", header = TRUE)
TestMCARNormality(datAdultAnalysis)

# No missing data for treatment
describe.factor(datAdultAnalysis$Treatment)
head(datAdultAnalysis)
```
Now impute the missing data
```{r}
head(datAdultAnalysis)

head(datAdultAnalysisComplete)

head(datAdultAnalysis)

m = 10
datAdultAnalysisImpute = amelia(m = m, datAdultAnalysis, noms = c("Gender", "Race", "Edu", "SexualOrientation", "RelationshipStatus", "Employment"), idvars = c("ID", "Treatment"), ts = "Time")

compare.density(datAdultAnalysisImpute, var = "RASTotalScore")
compare.density(datAdultAnalysisImpute, var = "INQTotalScore")
compare.density(datAdultAnalysisImpute, var = "SSMITotalScore")
compare.density(datAdultAnalysisImpute, var = "SISTotalScore")

summary(datAdultAnalysisImpute)

datAnalysisAll = lapply(1:m, function(x){datAdultAnalysisImpute$imputations[[x]]})
```

Check that outcomes variables are normal (just do histograms)
These are not that great may need some transofmrations 
```{r}
hist(datAdultAnalysisComplete$RASTotalScore)
hist(datAdultAnalysisComplete$INQTotalScore)
hist(datAdultAnalysisComplete$SSMITotalScore)
hist(datAdultAnalysisComplete$SISTotalScore)


```
Create a difference scores for each outcome
```{r}
head(datAdultAnalysisCompleteT.testComplete)

datAdultAnalysisCompleteT.testComplete$RASTotalT.Test = datAdultAnalysisCompleteT.testComplete$RASTotalScore.y-datAdultAnalysisCompleteT.testComplete$RASTotalScore.x

test = aov(datAdultAnalysisCompleteT.testComplete$RASTotalT.Test ~ datAdultAnalysisCompleteT.testComplete$Treatment.x)
summary(test)

compmeans(datAdultAnalysisCompleteT.testComplete$RASTotalT.Test, datAdultAnalysisCompleteT.testComplete$Treatment.x)

```



T-tests, non-par, and Cohen'sD
```{r}
# RAS
t.test(datAdultAnalysisCompleteT.testComplete$RASTotalScore.y, datAdultAnalysisCompleteT.testComplete$RASTotalScore.x, paried = TRUE)

wilcox.test(datAdultAnalysisCompleteT.testComplete$RASTotalScore.y, datAdultAnalysisCompleteT.testComplete$RASTotalScore.x, paried = TRUE)

cohen.d(datAdultAnalysisCompleteT.testComplete$RASTotalScore.y, datAdultAnalysisCompleteT.testComplete$RASTotalScore.x)

#INQ
t.test(datAdultAnalysisCompleteT.testComplete$INQTotalScore.y, datAdultAnalysisCompleteT.testComplete$INQTotalScore.x, paried = TRUE)

wilcox.test(datAdultAnalysisCompleteT.testComplete$INQTotalScore.y, datAdultAnalysisCompleteT.testComplete$INQTotalScore.x, paried = TRUE)


cohen.d(datAdultAnalysisCompleteT.testComplete$INQTotalScore.y, datAdultAnalysisCompleteT.testComplete$INQTotalScore.x)

#SSMI
t.test(datAdultAnalysisCompleteT.testComplete$SSMITotalScore.y, datAdultAnalysisCompleteT.testComplete$SSMITotalScore.x, paried = TRUE)

wilcox.test(datAdultAnalysisCompleteT.testComplete$SSMITotalScore.y, datAdultAnalysisCompleteT.testComplete$SSMITotalScore.x, paried = TRUE)

cohen.d(datAdultAnalysisCompleteT.testComplete$SSMITotalScore.y, datAdultAnalysisCompleteT.testComplete$SSMITotalScore.x)
```
###################################
Model with only time and treatment 
```{r}
modelRASCompleteTimeTreatment =  lmer(RASTotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatment)

modelINQCompleteTimeTreatment =  lmer(INQTotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatment)

modelSSMICompleteTimeTreatment =  lmer(SSMITotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatment)

modelSISCompleteTimeTreatment =  lmer(SISTotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatment)

```
###################################
Model with only time, treatment, and other covariates 
```{r}
head(datAdultAnalysisComplete)



modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + Age  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)


modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + Gender  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)



modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + Race  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)


modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + SexualOrientation  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)

modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)

modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)

modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + Edu  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)


modelRASCompleteTimeTreatmentCovars =  lmer(RASTotalScore ~ Time*factor(Treatment) + Employment  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelRASCompleteTimeTreatmentCovars)


#### INQ

modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + Age  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)


modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + Gender  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)



modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + Race  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)


modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + SexualOrientation  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)

modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)

modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)

modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + Edu  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)


modelINQCompleteTimeTreatmentCovars =  lmer(INQTotalScore ~ Time*factor(Treatment) + Employment  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelINQCompleteTimeTreatmentCovars)

### SSMI
modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + Age  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)


modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + Gender  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)



modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + Race  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)


modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + SexualOrientation  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)

modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)

modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)

modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + Edu  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)


modelSSMICompleteTimeTreatmentCovars =  lmer(SSMITotalScore ~ Time*factor(Treatment) + Employment  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSSMICompleteTimeTreatmentCovars)


### SIS

modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + Age  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)


modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + Gender  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)



modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + Race  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)


modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + SexualOrientation  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)

modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)

modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + RelationshipStatus  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)

modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + Edu  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)


modelSISCompleteTimeTreatmentCovars =  lmer(SISTotalScore ~ Time*factor(Treatment) + Employment  + (1 | ID), data  = datAdultAnalysisComplete)
summary(modelSISCompleteTimeTreatmentCovars)

```



########################
Imputed analysese below

Descriptives for base
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], Time == 0)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out
```
Descriptives for post
```{r}
datAnalysisAllDes = lapply(1:m, function(x){subset(datAnalysisAll[[x]], Time == 1)})

mean.out = NULL
for(i in 1:m) {
  mean.out[[i]] = apply(datAnalysisAllDes[[i]], 2, mean)
  mean.out = data.frame(mean.out)
}
mean.out

descFun = function(x){
  x = data.frame(t(x))
}
mean.out = descFun(mean.out)
mean.out

# now get sds
sd.out = NULL
for(i in 1:m) {
  sd.out[[i]] = apply(datAnalysisAllDes[[i]], 2, sd)
  sd.out = data.frame(sd.out)
}
sd.out = descFun(sd.out)
sd.out
mean.sd.out= mi.meld(mean.out, sd.out)
mean.sd.out

```
Need to get the pre and post scores for each variable separated then take the difference, then get the sd for them and divide by N

############################
T.tests with imputed data
############################

#############################
RAS Pre and Post, Treatment 1
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL

datAnalysisAll1 = datAnalysisAll[[1]]

for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 1)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 1)
  PreVar[[i]] = PreAll[[i]]$RASTotalScore
  PostVar[[i]] = PostAll[[i]]$RASTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
RAS Pre and Post, Treatment 2
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL

datAnalysisAll1 = datAnalysisAll[[1]]

for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 2)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 2)
  PreVar[[i]] = PreAll[[i]]$RASTotalScore
  PostVar[[i]] = PostAll[[i]]$RASTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
RAS Pre and Post, Treatment 3
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL

datAnalysisAll1 = datAnalysisAll[[1]]

for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 3)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 3)
  PreVar[[i]] = PreAll[[i]]$RASTotalScore
  PostVar[[i]] = PostAll[[i]]$RASTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
INQ Pre and Post, Treatment 1
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 1)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 1)
  PreVar[[i]] = PreAll[[i]]$INQTotalScore
  PostVar[[i]] = PostAll[[i]]$INQTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
INQ Pre and Post, Treatment 2
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 2)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 2)
  PreVar[[i]] = PreAll[[i]]$INQTotalScore
  PostVar[[i]] = PostAll[[i]]$INQTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
INQ Pre and Post, Treatment 3
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 3)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 3)
  PreVar[[i]] = PreAll[[i]]$INQTotalScore
  PostVar[[i]] = PostAll[[i]]$INQTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
SSMI Pre and Post, Treatment 1
Not significant
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 1)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 1)
  PreVar[[i]] = PreAll[[i]]$SSMITotalScore
  PostVar[[i]] = PostAll[[i]]$SSMITotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
SSMI Pre and Post, Treatment 2
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 2)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 2)
  PreVar[[i]] = PreAll[[i]]$SSMITotalScore
  PostVar[[i]] = PostAll[[i]]$SSMITotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
SSMI Pre and Post, Treatment 3
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 3)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 3)
  PreVar[[i]] = PreAll[[i]]$SSMITotalScore
  PostVar[[i]] = PostAll[[i]]$SSMITotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
SIS Pre and Post, Treatment 1
Not significant
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 1)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 1)
  PreVar[[i]] = PreAll[[i]]$SISTotalScore
  PostVar[[i]] = PostAll[[i]]$SISTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
SIS Pre and Post, Treatment 2
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 2)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 2)
  PreVar[[i]] = PreAll[[i]]$SISTotalScore
  PostVar[[i]] = PostAll[[i]]$SISTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```
#############################
SIS Pre and Post, Treatment 3
```{r}
T_Test = NULL
PreAll = NULL
PostAll = NULL
PreVar = NULL
PostVar = NULL
coef_output = NULL
# I think we divide by two, because it is the sample size for each group?
n = dim(datAnalysisAll[[1]])[1]/2
se_output = NULL


for(i in 1:m){
  PreAll[[i]] = subset(datAnalysisAll[[i]], Time == 0)
  PreAll[[i]] = subset(PreAll[[i]], Treatment == 3)
  PostAll[[i]] = subset(datAnalysisAll[[i]], Time == 1)
  PostAll[[i]] = subset(PostAll[[i]], Treatment == 3)
  PreVar[[i]] = PreAll[[i]]$SISTotalScore
  PostVar[[i]] = PostAll[[i]]$SISTotalScore
  coef_output[[i]] = mean(PostVar[[i]]- PreVar[[i]])
  se_output[[i]] = sd(PreVar[[i]]-PostVar[[i]])/sqrt(n)
}


coef_output = data.frame(coef_output)
se_output = data.frame(se_output)

# Figure out the degrees of freedom 


meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  t_stat = coefs1/ses1
  p = 2*pt(-abs(t_stat), df = n-1)
  return(data.frame(coefs1, ses1, t_stat, p))
}

results = meldAllT_stat(coef_output, se_output)
round(results,3) 
###################

```


############################
Multilevel models with imputed data
###############################
Now run model with just treatment and see what we have
RAS first
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL

for(i in 1:m){
  output[[i]] = lmer(RASTotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
RAS constrasts
```{r}
K = matrix(c(rep(0,4), 1, -1), 1)

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputReg[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
Now try INQ
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(INQTotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3) 
```
INQ constrasts
```{r}
K = matrix(c(rep(0,4), 1, -1), 1)

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputReg[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```

Now try SSMI
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(SSMITotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)

```
SSMI constrasts
```{r}
K = matrix(c(rep(0,4), 1, -1), 1)

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputReg[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```

Now try SIS
```{r}
output = list()
outputReg = list()
coef_output =  NULL
se_output = NULL
rSquared = NULL
df = NULL

for(i in 1:m){
  output[[i]] = lmer(SISTotalScore ~ Time*factor(Treatment) + (1 | ID), data  = datAnalysisAll[[i]])
  outputReg[[i]] = output[[i]]
  rSquared[[i]] = r.squaredGLMM(output[[i]])
  output[[i]] = summary(output[[i]])
  coef_output[[i]] = output[[i]]$coefficients[,1]
  se_output[[i]] = output[[i]]$coefficients[,2]
}
coef_output = data.frame(coef_output)
coef_output
quickTrans = function(x){
  x = data.frame(x)
  x = t(x)
  x = data.frame(x)
}
coef_output = quickTrans(coef_output)
coef_output
se_output = quickTrans(se_output)

# Figure out the degrees of freedom 

#coefsAll = mi.meld(q = coef_output, se = se_output)

meldAllT_stat = function(x,y){
  coefsAll = mi.meld(q = x, se = y)
  coefs1 = t(data.frame(coefsAll$q.mi))
  ses1 = t(data.frame(coefsAll$se.mi))
  z_stat = coefs1/ses1
  p = 2*pnorm(-abs(z_stat))
  return(data.frame(coefs1, ses1, z_stat, p))
}

results = meldAllT_stat(coef_output, se_output); results
round(results,3)

```

SIS constrasts
```{r}
K = matrix(c(rep(0,4), 1, -1), 1)

t = NULL
for(i in 1:m){
  t[[i]] = glht(outputReg[[i]], linfct = K)
  t[[i]] = summary(t[[i]])
}
t
```
##############################################
Ok no differences between any of the treatments to try simplier model for each treatment, pre and post paired for each treatment
```{r}

```
