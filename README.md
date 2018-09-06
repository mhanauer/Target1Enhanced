---
title: "TLC Data Analysis"
output: ''
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library the packages that you need
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
```


Ok so I need to transpose the data.  Use row.names = NULL forces numbering, so that will be used when we transpose it

Then grab the variables that we want.  We want the id, treatment, section 1, sections 1 through 4 and then demographics

Then we are renaming every variable. 

Clean the adult data set first then youth
```{r}
#rm(list=ls())
#setwd("P:/Evaluation/TN Lives Count_Writing/4_Target1_EnhancedCrisisFollow-up/3_Data & Data Analyses")
#datPreAdult = read.csv("Target1EnhancedBaseAdult.csv", header = TRUE)
#datPostAdult = read.csv("Target1EnhancedPostAdult.csv", header = TRUE)
#datPreYouth = read.csv("Target1EnhancedBaseYouth.csv", header= FALSE, row.names = NULL)
#datPostYouth = read.csv("Target1EnhancedPostYouth.csv", header = FALSE, row.names = NULL)

head(datPreAdult)
# subset the variables that you want
datPreAdult =datPreAdult[c(1, 3:4, 6,8,10, 12:13, 15:34, 36:45, 47:51, 53:59)]
head(datPreAdult)

head(datPostAdult)
datPostAdult = datPostAdult[c(1, 3:22,24:33, 35:39, 41:47)]
head(datPostAdult)

# Rename added variables otherwise everything else should be the same
colnames(datPreAdult)[colnames(datPreAdult) == "Added.V2..Thinking.of.Ways.to.Kill.Self"] = "Added"   

## Now merge everything
datAdult = merge(datPreAdult, datPostAdult, by = "Adult.ID")
head(datAdult)


```
Now we need to merge the treatment variable before we transform into long format to avoid duplication.
```{r}
# Need to er
#datAdultTreat = read.csv("AdultTreatment.csv", header = TRUE)
head(datAdultTreat)

datAdult = merge(datAdult, datAdultTreat, by = "Adult.ID", all.x = TRUE, sort = TRUE)
```


Now put into long format
```{r}
## Now long format so don't have to rename twice

datAdult = reshape(datAdult, varying = list(c("Desire.to.succeed.x", "Desire.to.succeed.y"), c("My.own.plan.to.stay.well.x", "My.own.plan.to.stay.well.y"), c("Goals.in.life.x", "Goals.in.life.y"), c("Believe.I.can.meet.personal.goals.x", "Believe.I.can.meet.personal.goals.y"), c("Purpose.in.life.x", "Purpose.in.life.y"), c("Fear.doesn.t.stop.me......x", "Fear.doesn.t.stop.me......y"), c("I.can.handle.my.life.x", "I.can.handle.my.life.y"), c("I.like.myself.x", "I.like.myself.y"), c("If.people.really.knew.me.......x", "If.people.really.knew.me.......y"), c("Who.I.want.to.become.x", "Who.I.want.to.become.y"), c("Something.good.will.happen.x", "Something.good.will.happen.y"), c("I.m.hopeful.about.future.x", "I.m.hopeful.about.future.y"), c("Continue.to.have.new.interests.x", "Continue.to.have.new.interests.y"), c("Coping.with.mental.illness.not.focus.of.life.x", "Coping.with.mental.illness.not.focus.of.life.y"), c("Symptoms.interfere.less.and.less.x", "Symptoms.interfere.less.and.less.y"), c("Symptoms.problem.for.shorter.periods.x", "Symptoms.problem.for.shorter.periods.y"), c("Know.when.to.ask.for.help.x", "Know.when.to.ask.for.help.y"), c("Willing.to.ask.for.help.x", "Willing.to.ask.for.help.y"), c("I.ask.for.help.when.I.need.it..x", "I.ask.for.help.when.I.need.it..y"), c("I.can.handle.stress..x", "I.can.handle.stress..y"), c("Better.off.if.I.were.gone.x", "Better.off.if.I.were.gone.y"), c("Happier.without.me.x", "Happier.without.me.y"), c("Death.would.be.a.relief.x", "Death.would.be.a.relief.y"), c("Wish.they.could.be.rid.of.me.x", "Wish.they.could.be.rid.of.me.y"), c("Make.things.worse.x", "Make.things.worse.y"), c("Feel.like.I.belong.x", "Feel.like.I.belong.y"), c("Have.many.caring.and.supportive.friends.x", "Have.many.caring.and.supportive.friends.y"), c("Feel.disconnected.x", "Feel.disconnected.y"), c("Feel.like.an.outsider.x", "Feel.like.an.outsider.y"), c("Close.to.other.people.x", "Close.to.other.people.y"), c("Unable.to.take.care.of.self.x", "Unable.to.take.care.of.self.y"), c("Not.recover.or.get.better.x", "Not.recover.or.get.better.y"), c("I.am.to.blame.x", "I.am.to.blame.y"), c("Unpredictable.x", "Unpredictable.y"), c("Dangerous.x", "Dangerous.y"), c("Wish.life.would.end..x", "Wish.life.would.end..y"), c("Life.not.worth.living.x", "Life.not.worth.living.y"), c("Life.so.bad..feel.like.giving.up..x", "Life.so.bad..feel.like.giving.up..y"), c("Better.for.everyone.if.I.were.to.die..x", "Better.for.everyone.if.I.were.to.die..y"), c("Added.x", "Added.y"), c("No.solution.to.my.problems.x", "No.solution.to.my.problems.y"), c("Believe.my.life.will.end.in.suicide..x", "Believe.my.life.will.end.in.suicide..y")), direction = "long", times = c(0,1))

head(datAdult)

dim(datAdult)
# Rename everything, which you will rename for the youth data set as well 
# 
colnames(datAdult) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "Treatment", "Time", "RAS1", "RAS2", "RAS3", "RAS4", "RAS5", "RAS6", "RAS7", "RAS8", "RAS9", "RAS10", "RAS11", "RAS12", "RAS13", "RAS14", "RAS15", "RAS16", "RAS17", "RAS18", "RAS19", "RAS20", "INQ1", "INQ2", "INQ3", "INQ4", "INQ5", "INQ6", "INQ7", "INQ8", "INQ9", "INQ10", "SSMI1", "SSMI2", "SSMI3", "SSMI4", "SSMI5", "SIS1", "SIS2", "SIS3", "SIS4", "SIS5", "SIS6", "SIS7")

# Drop last column id 
datAdult = data.frame(datAdult)
head(datAdult)
datAdult$NA. = NULL
head(datAdult)
dim(datAdult)
head(datAdult)
```
Checking for issues with adult data set
```{r}
summary(datAdult)

# One person age is 451 get rid of them
datAdult = subset(datAdult, Age < 450)
dim(datAdult)

# Two treatments have B with space first so try and recode those as just B's
datAdult$Treatment = ifelse(datAdult$Treatment == " B", "B", datAdult$Treatment)
datAdultTest
describe.factor(datAdult$Treatment)

describe.factor(datAdult$Treatment)
datAdult$Treatment = ifelse(datAdult$Treatment == 8, 1, ifelse(datAdult$Treatment == 3, 2, ifelse(datAdult$Treatment == 6, 3, ifelse(datAdult$Treatment == "B", 2, datAdult$Treatment))))

describe.factor(datAdult$Treatment)

## If there are NA's for treatment then need to drop them
datAdult = subset(datAdult, Treatment == 1 | Treatment == 2 | Treatment == 3)
```
Ok now get descriptives for pre and post
```{r}
datAdultPre = subset(datAdult, Time == 0)
describe(datAdultPre)

datAdultPost = subset(datAdult, Time == 1)
describe(datAdultPost)
```
Now get psychometrics for the baseline data
```{r}
head(datAdultPre)
RAS = datAdultPre[c(11:30)]
RAS

INQ

head(datAdultPre)

INQ = datAdultPre[c(31:40)]
head(INQ)

# Three items are reversed scored: f = 6, g = 7, j = 10
datAdultPre$INQ6 = ifelse(datAdultPre$INQ6 == 1, 5, ifelse(datAdultPre$INQ6 == 2,4, ifelse(datAdultPre$INQ6  == 3,3, ifelse(datAdultPre$INQ6  == 4,2, ifelse(datAdultPre$INQ6  == 5,1,datAdultPre$INQ6)))))

datAdultPre$INQ7= ifelse(datAdultPre$INQ7== 1, 5, ifelse(datAdultPre$INQ7== 2,4, ifelse(datAdultPre$INQ7 == 3,3, ifelse(datAdultPre$INQ7 == 4,2, ifelse(datAdultPre$INQ7 == 5,1,datAdultPre$INQ7)))))

datAdultPre$INQ10= ifelse(datAdultPre$INQ10== 1, 5, ifelse(datAdultPre$INQ10== 2,4, ifelse(datAdultPre$INQ10 == 3,3, ifelse(datAdultPre$INQ10 == 4,2, ifelse(datAdultPre$INQ10 == 5,1,datAdultPre$INQ10)))))


SSMI = datAdultPre[c(41:45)]
head(SSMI)

SIS = datAdultPre[c(47:52)]
SIS

```
Now run psychometrics on each of them
```{r}
# RAS overall

omegaRAS = omega(RAS)
summary(omegaRAS)

# Get reliabilities for the subscales
# Subscale one: f =6, g = 7, h = 8, I = 9,  j = 10, k = 11, l = 12, m = 13, t = 20
head(RAS)
RASSub1 = RAS[c(6:13, 20)]

omegaRASSub1 =  omega(RASSub1)
summary(omegaRASSub1)

# Subscale two q = 17, r= 18, s= 19
head(RAS)
RASSub2 = RAS[c(17:19)]

omegaRASSub2 =  omega(RASSub2)
summary(omegaRASSub2)

# Subscale three: a = 1, b = 2, c = 3, d= 4, e = 5
head(RAS)
RASSub3 = RAS[c(1:5)]

omegaRASSub3 =  omega(RASSub3)
summary(omegaRASSub3)

# Subscale five: n = 14, o = 15, p = 16
head(RAS)
RASSub5 = RAS[c(14:16)]

omegaRASSub5 =  omega(RASSub5)
summary(omegaRASSub5)



```




Now need to start cleaning the youth data set start with pre first

INQ for youth is not the same, maybe drop it when merging and not worry about this measure for merged data 
```{r}
datPreYouth = t(datPreYouth)
write.csv(datPreYouth, "datPreYouth.csv", row.names = FALSE)
datPreYouth = read.csv("datPreYouth.csv", header = TRUE)
head(datPreYouth)
# Get rid of variable one so the count is right
datPreYouth$X = NULL
head(datPreYouth)
dim(datPreYouth)
RelationshipStatus = rep(NA, 362)
Edu = rep(NA, 362)
Employment = rep(NA, 362)
datPreYouth$RelationshipStatus = RelationshipStatus
datPreYouth$Edu = Edu
datPreYouth$Employment = Employment
head(datPreYouth)
# Need to add in relationship status, education, and employment as blanks to merge
datPreYouth = datPreYouth[c(4, 10,11, 13, 15, 82:84, 7, 20:39, 56:60, 63:69)]
head(datPreYouth)
dim(datPreYouth)


colnames(datPreYouth) = c("ID", "Age", "Gender", "Race", "SexualOrientation", "RelationshipStatus", "Edu", "Employment", "Treatment", "RAS1", "RAS2", "RAS3", "RAS4", "RAS5", "RAS6", "RAS7", "RAS8", "RAS9", "RAS10", "RAS11", "RAS12", "RAS13", "RAS14", "RAS15", "RAS16", "RAS17", "RAS18", "RAS19", "RAS20", "SSMI1", "SSMI2", "SSMI3", "SSMI4", "SSMI5", "SIS1", "SIS2", "SIS3", "SIS4", "SIS5", "SIS6", "SIS7")

datPreYouth = datPreYouth[-1,]

head(datPreYouth)
```
Now youth post scores
```{r}
datPostYouth = t(datPostYouth)
write.csv(datPostYouth, "datPostYouth.csv", row.names = FALSE)
datPostYouth = read.csv("datPostYouth.csv", header = TRUE)
head(datPostYouth)
# Get rid of variable one so the count is right
dim(datPostYouth)
head(datPostYouth)
# Need to add in relationship status, education, and employment as blanks to merge
datPostYouth = datPostYouth[c(4,11:30,47:51, 54:60)]
head(datPostYouth)
dim(datPostYouth)


colnames(datPostYouth) = c("ID", "RAS1", "RAS2", "RAS3", "RAS4", "RAS5", "RAS6", "RAS7", "RAS8", "RAS9", "RAS10", "RAS11", "RAS12", "RAS13", "RAS14", "RAS15", "RAS16", "RAS17", "RAS18", "RAS19", "RAS20", "SSMI1", "SSMI2", "SSMI3", "SSMI4", "SSMI5", "SIS1", "SIS2", "SIS3", "SIS4", "SIS5", "SIS6", "SIS7")

datPostYouth = datPostYouth[-1,]

head(datPostYouth)


```
