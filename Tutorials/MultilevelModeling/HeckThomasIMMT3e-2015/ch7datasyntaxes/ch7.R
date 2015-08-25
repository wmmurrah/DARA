#**************************************************************************
# Chapter 7: Individual and Organizational Change -------------------------
# Author: William Murrah
# Description: R Code for chapter 7 of:
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
library(sjPlot)
library(lattice)
library(mosaic)

grad <- read.table("ch7grad1.txt", header = FALSE)
names(grad) <- c("id", "grad1", "grad2", "grad3", "grad4",
                 "private", "prestige")
stargazer(grad, type = "text")
# Using reshape() to transform data from wide to long.
gradlong <- reshape(data = grad,                   # Wide data frame.
                    varying = c("grad1", "grad2",  # Time-varying variables,
                                "grad3", "grad4"), #   only one here.
                    v.names = "graduate",          # Name for stacked variable.
                    timevar = "growrate",          # Name for timing variable.
                    times = 0:3,                   # Values for timing variable.
                    idvar = "id",                  # Id variable.
                    direction = "long")            # Indicate going to long.
# Rename rownames
rownames(gradlong) <- 1:nrow(gradlong)
# Reorder data frame by 'id' variable.
gradlong <- gradlong[order(gradlong$id) , ]

# I checked this gradlong with the Mplus version.
# ch7ex1 <- read.table("ch7ex1.dat", header = FALSE)
# 
# names(ch7ex1) <- c("id", "private", "prestige", "index1", "graduate", "growrate")
# ch7ex1 <- ch7ex1[ ,-4]
# ch7ex1 <- ch7ex1[ c(1,2,3,5,4)]

mean(graduate, data = gradlong, groups = growrate)
sd(graduate, data = gradlong, groups = growrate)

# Plot individual growth trajectories with lattice package.
xyplot(graduate ~ growrate , 
       data = gradlong, 
       group =id, 
       type = "b",
       col = "black")

mod0 <- lmer(graduate ~ 1 + (1 |id), gradlong, REML = FALSE)
screenreg(mod0)
# Calculate ICC.
252.03/(252.03 + 112.75)

mod1 <- lmer(graduate ~ growrate + (growrate | id), 
             data = gradlong, REML = FALSE)
screenreg(list(mod0, mod1))

ctr <- function(x) scale(x, scale = FALSE)
gradlong$prestige <- ctr(gradlong$prestige)
gradlong$private <- ctr(gradlong$private)
mod2 <- lmer(graduate ~ growrate*private + growrate*prestige + (growrate | id),
             data = gradlong, REML = FALSE)
screenreg(list(mod0, mod1, mod2))



# Example with varying occasions ------------------------------------------

curran <- read_spss("CurranLong.sav")

view_df(curran, showFreq = TRUE, showPerc = T)
stargazer(curran, type = "text")

# How many children are in our data?
length(unique(curran$id))

xyplot(read ~ kidagetv, 
       data = curran, 
       groups = id, 
       type = "b",
       col = "black")

mod0 <-   lmer(read ~ poly(kidage6, 2, raw = TRUE) + (1 | id), 
               curran, REML = FALSE)
screenreg(mod0)
mod0.2 <- lmer(read ~ kidage6 + kidagesq + (kidage6 | id), curran, 
               REML = FALSE)
screenreg(list(mod0, mod0.2))


### Varying occasions in Mplus

library(MplusAutomation)

prepareMplusData(df = curran, 
                 filename = "curran.dat", 
                 inpfile = "curran.inp")
