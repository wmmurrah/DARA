#**************************************************************************
# Title:         Core1 ITT ------------------------------------------------
# Description    ITT analyses for students applying to only one study 
#                school.
# Author:        William Murrah
#**************************************************************************
# Version history ---------------------------------------------------------
# 2014.01.13 Created
#**************************************************************************
# data and packages used: -------------------------------------------------

# Load data if not already in global environment.
if (exists('core1')==FALSE) source("data/core1Rmake.R")

# Load function to run ITT regressions.
source('R/ittReg.R')

# Define arguments to pass to ittReg function.
df <- core1
outcomes   <- c('ppvt1', 'wj.lwid1', 'wj.passcomp1', 
                'wjak.science1', 'wjak.socstud1', 'wjak.hum1',
                'wjak.tot1')
tx.assign  <- 'lottery'
covariates <- c('school')

# Initialize empty list
ckcs.itt <- list()

# Loop over variables in the 'outcomes' vector using ittReg function.
# Results in a list of model objects.
for(i in 1:length(outcomes)){
  outcome <- outcomes[i]
  ckcs.itt[[i]] <- ittReg()
}

names(ckcs.itt) <- outcomes


# END ---------------------------------------------------------------------
#**************************************************************************