#**************************************************************************
# Package `haven` Demonstration -------------------------------------------
# Author: William Murrah
#**************************************************************************
library(mosaic)
# Load `haven` package
library(haven)

# source custom factor function
source("R/as_factor_lbl.R")
# Read SPSS file
demog <- read_spss("data/Dem_ParQuant_Summer_2013_C2.sav")

str(demog$COND_1)
table(demog$COND_1)

# Convert to factor
demog$COND_1 <- as_factor_lbl(demog$COND_1)

table(demog$COND_1)

demog$foo <- factor(rep(0:1, nrow(demog)/2), labels=c("No", "Yes"))
attr(demog$foo, "label") <- "This is a fake factor variable"

demog$bar <- rnorm(nrow(demog))
attr(demog$bar, "label") <- "This is a fake numeric variable"

table(demog$foo, useNA = 'always')

favstats(demog$bar)


# write SPSS file from R data.
write_sav(demog, "data/demogMOD.sav")


str(demog$COHORT)

lbl <- attr(demog$COHORT, "label")

demog$COHORT <- as_factor(demog$COHORT)
attr(demog$COHORT, 'label') <- lbl

str(demog$COHORT)


write_sav(demog, "data/demogMOD.sav")
