#**************************************************************************
# Chapter 3 ---------------------------------------------------------------
# Author: William Murrah
# Description: R Code for chapter 3 of:
#              Heck, R. H., & Thomas, S. L. (2015). An Introduction to 
#              Multilevel Modeling Techniques: MLM and SEM Approaches Using 
#              Mplus. Routledge.
#**************************************************************************
rm(list=ls())

# Packages used -----------------------------------------------------------
library(lme4)
library(texreg)
library(stargazer)
library(psychometric)
library(psych)
library(sjmisc)

# User defined functions used ---------------------------------------------

# Reliability function
lam.rel <- function(var.b, var.w, nj.vec) {
  lam <- var.b/(var.b + (var.w/nj.vec))
  return(lam)
}

# Center function.
ctr <- function(x) scale(x, scale = FALSE)
# Group mean center function.
gmc <- function(x, grp) {
  return(x-tapply(x, grp, mean, na.rm = TRUE)[grp])
}
# Load data ---------------------------------------------------------------
ch3new <- read.table("ch3new.dat", 
                     header = FALSE)

names(ch3new) <- c("deptid", "morale", "satpay", "female", "white", "pctbelow", 
                   "lev1wt", "lev2wt")


# I keep all variable as numeric for now
stargazer(ch3new, type = "text",
          title = "Level 1 descriptive statistics (talk about data)")

# How many groups are there (deptid)?
length(unique(ch3new$deptid))
# What is the range of group sizes?
range(table(ch3new$deptid))
# What is the average group size?
mean(table(ch3new$deptid))

# Recode categorical variables into factors.
ch3new$deptid <- to_fac(ch3new$deptid)
ch3new$female <- to_fac(ch3new$female)
ch3new$white <- to_fac(ch3new$white)

agg.pctbelow <- aggregate(formula = pctbelow ~ deptid, 
                          data = ch3new, 
                          FUN = mean)
mean(agg.pctbelow[ ,2])
sd(agg.pctbelow[ ,2])

morale.aov <- aov(morale ~ deptid, ch3new)
summary(morale.aov)

# Model 1: Unconditional Model --------------------------------------------
model1 <- lmer(morale ~ 1 + (1 | deptid), ch3new, REML = FALSE)
summary(model1)
screenreg(model1)

# Useful information:
# 1. Estimated mean morale score (Intercept).
# 2. Partitioning of total variance in morale score betweem level 1 
#    and level2.
# 3. Level of dependence within level 2 units (ICC).
#    between/(between + within) = between/total
# ICC
ICC1.lme(morale, deptid, ch3new)
# 4. Reliability of each department's mean morale score can be estimated.

# lam.rel <- function(var.b, var.w, nj.vec) {
#   lam <- var.b/(var.b + (var.w/nj.vec))
#   return(lam)
# }

lam.rel(var.b = 5.4, 
        var.w = 33.30, 
        nj.vec = c(14, 80, 200))
# 5. Test of the null hypothesis that all departments have the same
# mean morale score.

####### Compare this to Mplus output ##########
# Look at ch3Model1.out.


# Centering ---------------------------------------------------------------
range(ch3new$satpay)
mean(ch3new$satpay)


# Random intercept with fixed level-1 slope:
ch3new$grMsatpay <- tapply(ch3new$satpay, 
                           ch3new$deptid, 
                           mean, na.rm = TRUE)[ch3new$deptid]
model2.no.ctr <- lmer(morale ~ satpay + (1 | deptid), ch3new) 
model2.Zx      <- lmer(morale ~ scale(satpay) + (1 | deptid), ch3new) 
model2.grand  <- lmer(morale ~ ctr(satpay) + (1 | deptid), ch3new) 
model2.group <- lmer(morale ~ gmc(satpay, deptid) + (1 | deptid), ch3new) 
model2.ZxL2     <- lmer(morale ~  gmc(satpay, deptid) + grMsatpay + 
                          (1 | deptid), ch3new) 
screenreg(list(model2.no.ctr, model2.Zx, model2.grand,
               model2.group, model2.ZxL2),
          custom.coef.names = c("(Intercept)", "satpay","satpay", 
                                "satpay", "satpay", "GroupMn satpay"),
          custom.model.names = c("Uncentered", "Scaled", "Grand", 
                                 "Group", "Group(+L2)"))

###
# Grand mean centering:
# 1. Interpreted as the expected value of the outcome when the predictor is at 
#    zero (the mean value of on the original scale).
# 2. While the mean is changed (it is 0 on the new scale), the standard 
#    deviation is the same.
sd(ch3new$satpay)
sd(ctr(ch3new$satpay))
# 3. With grand mean centering the level 2 intercept has been adjusted for 
#    the level-1 predictors.
# 4. The variance of intercept and slope are the variances of the "average"
#    person.

# Group mean centering:
# 1. Intercept is the expected value of the outcome when the predictor is at
#    zero, which is the individual's group mean.
# 2. GROUP MEAN CENTERING CHANGES THE MEANING OF THE MODEL!
# 3. Emphasis is on relational advantages within groups.
# 4. Information about differences across level-2 units is removed from the 
#    predictors.

# Random intercept and level-1 slope:
model2.no.ctr <- lmer(morale ~ satpay + (satpay | deptid), ch3new) 
model2.Zx      <- lmer(morale ~ scale(satpay) + (scale(satpay) | deptid), ch3new) 
model2.grand  <- lmer(morale ~ ctr(satpay) + (ctr(satpay) | deptid), ch3new) 
model2.group <- lmer(morale ~ gmc(satpay, deptid) + (gmc(satpay, deptid)| deptid), ch3new) 
model2.ZxL2     <- lmer(morale ~  gmc(satpay, deptid) + grMsatpay + 
                          (gmc(satpay, deptid) | deptid), ch3new) 
screenreg(list(model2.no.ctr, model2.Zx, model2.grand, 
               model2.group, model2.ZxL2), 
          custom.coef.names = c("(Intercept)", "satpay","satpay", 
                                "satpay", "satpay", "GroupMn satpay"),
          custom.model.names = c("Uncentered", "Scaled", "Grand", 
                                 "Group", "Group(+L2)"))

# Model 2: Random-intercept Model -----------------------------------------
# Now we build a level 1 model by considering theoretically relevant predictors.
# It is common to first consider predictors as fixed (e.g. the same across all 
# groups.)
#
# For our example we will consider satisifaction with pay (satpay), gender 
# (female), and race (white).
# Note that I do not center the dummy coded variables, which differs from the 
# book.

# Book model:
# model2 <- lmer(morale ~ ctr(satpay) +  ctr(female) + ctr(white) + (1 | deptid), 
#                ch3new)

# model2 <- lmer(morale ~ ctr(satpay) +  I(female - .50) + white + (1 | deptid), 
#                ch3new)
# How I would model this:
model2 <- lmer(morale ~ ctr(satpay) +  female + white + (1 | deptid), 
               ch3new, REML = FALSE)
# Note that the slope of all three predictors is fixed.

summary(model2)
screenreg(list(model1, model2))
# The interpretation of the intecept is the average morale for people with
# average satpay for nonwhite males. 
femodel2 <- lm( morale ~ ctr(satpay) + female + white + deptid, data = ch3new)
coef(femodel2)[1:5]
# ICC model 2
1.85/(1.85 + 17.54)

# ICC model 1
5.36/(5.36 + 33.3)
(.1385 - .0954)/.1385
# The total variance between departments is diminished about 31% by the 
#  variables in the model, compared with the unconditional model.

anova(model1, model2)

ch3new$grMsatpay <- tapply(ch3new$satpay, 
                           ch3new$deptid, 
                           mean, na.rm = TRUE)[ch3new$deptid]
model2.no.ctr <- lmer(morale ~ satpay + (1 | deptid), ch3new) 
model2.Zx      <- lmer(morale ~ scale(satpay) + (1 | deptid), ch3new) 
model2.grand  <- lmer(morale ~ ctr(satpay) + (1 | deptid), ch3new) 
model2.group <- lmer(morale ~ gmc(satpay, deptid) + (1 | deptid), ch3new) 
model2.ZxL2     <- lmer(morale ~  gmc(satpay, deptid) + ctr(grMsatpay) + 
                          (1 | deptid), ch3new) 
screenreg(list(model2.no.ctr, model2.Zx, model2.grand,
               model2.group, model2.ZxL2),
          custom.coef.names = c("(Intercept", "satpay","satpay", 
                                "satpay", "satpay", "GroupMn satpay"),
          custom.model.names = c("Uncentered", "Std.X", "Grand", 
                                 "Group", "Std.XY"))
# Model 3: Level 1 Random Slope Model -------------------------------------

model3 <- lmer(morale ~ ctr(satpay) + female + white + 
                 (1 + ctr(satpay) | deptid), ch3new, REML = FALSE)
summary(model3)
screenreg(list(model1, model2, model3))

anova(model2, model3)

#### Mplus #####
# In addition to changing the modle statement, we must change t
# he analysis type when we have random slopes:
#
# ANALYSIS: type = twolevel random;
#

# Model 4: Level-2 intercept and slope ------------------------------------

# Is a department's overall pay level associated with its average 
# morale level?

model4 <- lmer(morale ~ ctr(satpay)*ctr(pctbelow)  + female + white + 
                 (1 + ctr(satpay) | deptid), 
               ch3new, REML = FALSE)
summary(model4)
screenreg(list(model1, model2, model3, model4))
screenreg(list(model1, model4))
#### Mplus ####
# In addition to changes in the model statement, we must also:
#
# 1. add pctbelow to usevariables statement.
# 2. we must add pctbelow to %BETWEEN% statement.

### Variance explained by final model at each level:

# Within 
(33.30 - 17.46)/33.30

# Between
(5.36 - 1.70)/ 5.36
