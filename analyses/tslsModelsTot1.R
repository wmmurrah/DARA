#**************************************************************************
# Core Knowledge TSLS analyses: Risk set 1 --------------------------------
# Author: William Murrah
# Description: Two stage least squares estimates of the treatment-on-the-
#              treated effects for the Core Knowledge Charter School 
#              study. This analysis uses Risk set 1, which uses school
#              applied to as the risk set and includes only cases which
#              applied to one school.
#
# Version history ---------------------------------------------------------
# 2013.12.19: Created
#**************************************************************************


# data and packages used: -------------------------------------------------

source("data/core1Rmake.R")
library(AER)

fml <- "scale(Y) ~ enrolled + school + kfull | lottery + school + kfull"
# ppvt --------------------------------------------------------------------
Y <- core1$ppvt1
ppvt.iv1 <- ivreg(fml, data=core1)
#  summary(ppvt.iv1)

# Letter word ID ----------------------------------------------------------
Y <- core1$wj.lwid1
lwid.iv1 <- ivreg(fml, data=core1)

# passage comp ------------------------------------------------------------
Y <- core1$wj.passcomp1
passcomp.iv1 <- ivreg(fml, data=core1)

# science -----------------------------------------------------------------
Y <- core1$wjak.science1
sci.iv1 <- ivreg(fml, data=core1)

# social studies ----------------------------------------------------------
Y <- core1$wjak.socstud1
socstud.iv1 <- ivreg(fml, data=core1)

# humanities --------------------------------------------------------------
Y <- core1$wjak.hum1
hum.iv1 <- ivreg(fml, data=core1)

# AK total ----------------------------------------------------------------
Y <- core1$wjak.tot1
aktot.iv1 <- ivreg(fml, data=core1)

# summaries ---------------------------------------------------------------

# summary(ppvt.iv1)
# summary(lwid.iv1)
# summary(passcomp.iv1)
# summary(sci.iv1)
# summary(socstud.iv1)
# summary(hum.iv1)
# summary(aktot.iv1)

# library(texreg)
# screenreg(list(ppvt.iv1, lwid.iv1, passcomp.iv1, sci.iv1, socstud.iv1, 
#                hum.iv1, aktot.iv1),
#           custom.model.names=c("ppvt1", "wj.lwid1", "wj.passcomp1", 
#                                "wjak.science1", "wjak.socstud1", 
#                                "wjak.hum1", "wjak.tot1"),
#           stars=c(.001,.01,.05,.10),symbol="\\dagger")

# END ---------------------------------------------------------------------
#**************************************************************************

