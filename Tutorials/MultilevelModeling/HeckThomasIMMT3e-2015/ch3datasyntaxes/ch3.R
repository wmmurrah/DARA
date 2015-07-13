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

# Center function
ctr <- function(x) scale(x, scale = FALSE)

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
model1 <- lmer(morale ~ 1 + (1 | deptid), ch3new)
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

lam.rel <- function(var.b, var.w, nj.vec) {
  lam <- var.b/(var.b + (var.w/nj.vec))
  return(lam)
}

lam.rel(var.b = 5.4, 
        var.w = 33.30, 
        nj.vec = c(14, 80, 200))
# 5. Test of the null hypothesis that all departments have the same
# mean morale score.

####### Compare this to Mplus output ##########


# Model 2: Random-intercept Model -----------------------------------------

# model2 <- lmer(morale ~ ctr(satpay) +  ctr(female) + ctr(white) + (1 | deptid), 
#                ch3new)
model2 <- lmer(morale ~ ctr(satpay) +  female + white + (1 | deptid), 
               ch3new)
summary(model2)
screenreg(list(model1, model2))


# Model 3: Level 1 Random Slope Model -------------------------------------

model3 <- lmer(morale ~ ctr(satpay) + female + white + 
                 (1 + ctr(satpay) | deptid), ch3new)
summary(model3)
screenreg(list(model1, model2, model3))


# Model 4: Level-2 intercept and slope ------------------------------------

model4 <- lmer(morale ~ ctr(satpay) + ctr(female) + ctr(white) + ctr(pctbelow) +
                 (1 + ctr(satpay) + ctr(pctbelow) | deptid), ch3new)
summary(model4)
screenreg(list(model1, model2, model3, model4))
