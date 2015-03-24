#**************************************************************************
# Core Knowledge TSLS analyses: Risk set 1 --------------------------------
# Author: William Murrah
# Description: Two stage least squares estimates of the treatment-on-the-
#              treated effects for the Core Knowledge Charter School 
#              study. This analysis prepares results for the AEFP conference.
#
# Version history ---------------------------------------------------------
# 2013.12.19: Created
# 2014.11.18: Created totReg function and looped analysis file.
#**************************************************************************


# data and packages used: -------------------------------------------------

if (!exists('core1')) source("data/core1Rmake.R")

source('R/totReg.R')
source('R/ittReg.R')

df <- core1
# remove Platte River from data.
# df <- core1[core1$school != "pra", ]

# Create tx receipt variable tot3, changing 14 cases on platte river
# that are control crossovers
# df$tot3 <- df$tot.currentID
# df$tot3[(core1$lottery=="Not Offered" & core1$tot.currentID==1 & 
#   core1$school=="pra")] <- 0


outcomes   <- c('wj.lwid1', 'ppvt1', 'wjak.tot1', 'wj.passcomp1')  

instrument <- tx.assign <- 'lottery'

# instrument <-  c('acsLottery', 'amaLottery', 'lcaLottery',
#                 'nvcLottery', 'praLottery')
# treatment  <- 'tot.ks.st'
treatment   <- 'tot.study'
# treatment <- 'tot3'
covariates <- 'school'

# ITT  --------------------------------------------------------------------
ckcs.itt <- list()

for(i in 1:length(outcomes)){
  outcome <- outcomes[i]
  ckcs.itt[[i]] <- ittReg()
}

names(ckcs.itt) <- outcomes

# TOT: Accepted Offer to Study School ------------------------------------
ckcs.tot2  <- list()

for(i in 1:length(outcomes)){
  outcome <- outcomes[i]
  ckcs.tot2[[i]] <- totReg()
}

names(ckcs.tot2) <- outcomes

# TOT: Attending Study School at end of 1st Grade -------------------------

treatment <- 'tot.currentID'

ckcs.tot1st  <- list()
ckcs.tot1st[[1]] <- lm(as.numeric(tot.currentID)-1 ~ lottery + school, data=df)
for(i in 1:length(outcomes)){
  outcome <- outcomes[i]
  ckcs.tot1st[[i]] <- totReg()
}

names(ckcs.tot1st) <- outcomes


# TOT: Attended any Core Knowledge Charter School -------------------------

treatment <- 'tot2'

ckcs.anyckcs  <- list()
ckcs.anyckcs[[1]] <- lm(as.numeric(tot.currentID)-1 ~ lottery + school, data=df)
for(i in 1:length(outcomes)){
  outcome <- outcomes[i]
  ckcs.anyckcs[[i]] <- totReg()
}

names(ckcs.anyckcs) <- outcomes


# View Results ------------------------------------------------------------

# library(texreg)
# screenreg(ckcs.itt)
# screenreg(ckcs.tot2)
# screenreg(ckcs.tot1st)

