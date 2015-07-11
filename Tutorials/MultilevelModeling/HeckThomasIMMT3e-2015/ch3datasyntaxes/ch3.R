#**************************************************************************
# Chapter 3 ---------------------------------------------------------------
# Author: William Murrah
# Description: R Code for chapter 3 of:
#              Heck, R. H., & Thomas, S. L. (2015). An Introduction to 
#              Multilevel Modeling Techniques: MLM and SEM Approaches Using 
#              Mplus. Routledge.
#**************************************************************************
library(lme4)
library(texreg)
library(stargazer)
library(psychometric)
library(psych)

# Load data
ch3new <- read.table("ch3new.dat", header = FALSE)

names(ch3new) <- c("deptid", "morale", "satpay", "female", "white", "pctbelow", 
                   "lev1wt", "lev2wt")

stargazer(ch3new, type = "text")

agg.pctbelow <- aggregate(formula = pctbelow ~ deptid, 
                          data = ch3new, 
                          FUN = mean)
mean(agg.pctbelow[ ,2])
sd(agg.pctbelow[ ,2])


# Model 1: Unconditional Model --------------------------------------------

model1 <- lmer(morale ~ 1 + (1 | deptid), ch3new)
summary(model1)
screenreg(model1)

# ICC
ICC1.lme(morale, deptid, ch3new)



# Model 2: Random-intercept Model -----------------------------------------

ctr <- function(x) scale(x, scale = FALSE)

model2 <- lmer(morale ~ ctr(satpay) +  ctr(female) + ctr(white) + (1 | deptid), 
               ch3new)
summary(model2)
screenreg(list(model1, model2))


# Model 3: Level 1 Random Slope Model -------------------------------------

model3 <- lmer(morale ~ ctr(satpay) + ctr(female) + ctr(white) + 
                 (1 + ctr(satpay) | deptid), ch3new)
summary(model3)
screenreg(list(model1, model2, model3))
