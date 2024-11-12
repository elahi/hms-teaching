################################################################################
##' @title Revisiting code from Elahi and Sebens 2012 MEPS
##' @author Robin Elahi
##' @contact elahi@stanford.edu
##' @date 2024-11-12
##' @log 
################################################################################

# Robin Elahi
# 7 April 2012
# Script for MEPS paper

#### File paths ####
here::i_am("sem_meps_2012/sem_meps_241112.R")
library(here)
folder <- "sem_meps_2012"
file_name <- "sem_meps_241112.R"

#### PACKAGES ####
library(lavaan)

#### DATA ####
survey <- read.csv(here(folder, "data", "survey_sem.csv"))
expt <- read.csv(here(folder, "data", "expt_sem.csv"))
surv_expt <- read.csv(here(folder, "data", "survey_expt_sem.csv"))

#### MODELS ####

# 4 models
# A.  Urchin facilitation, chiton grazing
# B.  No facilitation, chiton grazing
# C.  Urchin facilitation, no chiton grazing
# D.  No facilitation, no chiton grazing


# A.  Urchin facilitation, chiton grazing (saturated model)
mod.A <- '
# regressions
space.logit ~ urchin.ln
prey.rich ~ urchin.ln
space.logit ~ chiton.ln
prey.rich ~ chiton.ln
chiton.ln ~ urchin.ln
# covariances
space.logit ~~ prey.rich
' # end model

# B.  No facilitation, chiton grazing
mod.B <- '
# regressions
space.logit ~ urchin.ln
prey.rich ~ urchin.ln
space.logit ~ chiton.ln
prey.rich ~ chiton.ln
chiton.ln ~ 0*urchin.ln
# covariances
space.logit ~~ prey.rich
' # end model

# C.  Urchin facilitation, no chiton grazing
mod.C <- '
# regressions
space.logit ~ urchin.ln
prey.rich ~ urchin.ln
space.logit ~ 0*chiton.ln
prey.rich ~ 0*chiton.ln
chiton.ln ~ urchin.ln
# covariances
space.logit ~~ prey.rich
' # end model

# D.  No facilitation, no chiton grazing
mod.D <- '
# regressions
space.logit ~ urchin.ln
prey.rich ~ urchin.ln
space.logit ~ 0*chiton.ln
prey.rich ~ 0*chiton.ln
chiton.ln ~ 0*urchin.ln
# covariances
space.logit ~~ prey.rich
' # end model

#### Fit survey data ####
survAfit <- sem(model=mod.A, data=survey, estimator='MLM')
survBfit <- sem(model=mod.B, data=survey, estimator='MLM')
survCfit <- sem(model=mod.C, data=survey, estimator='MLM')
survDfit <- sem(model=mod.D, data=survey, estimator='MLM')
summary(survAfit)
summary(survBfit)
summary(survCfit)
summary(survDfit)

aic.txt <- c("Urchin facilitation, chiton grazing (saturated model)", "No facilitation, chiton grazing","Urchin facilitation, no chiton grazing", "No facilitation, no chiton grazing")

library(AICcmodavg)
source(here(folder, "lavaan.modavg.R"))
survey.aic <- aictab.lavaan(list(survAfit, survBfit, survCfit, survDfit), aic.txt)
survey.aic
 
summary(survAfit, fit.measures=TRUE, standardized=T,rsquare=T)
standardizedSolution(survAfit)

#### Fit experimental data ####
exptAfit <- sem(model=mod.A, data=expt, estimator='MLM')
exptBfit <- sem(model=mod.B, data=expt, estimator='MLM')
exptCfit <- sem(model=mod.C, data=expt, estimator='MLM')
exptDfit <- sem(model=mod.D, data=expt, estimator='MLM')

summary(exptAfit)
summary(exptBfit)
summary(exptCfit)
summary(exptDfit)

expt.aic <- aictab.lavaan(list(exptAfit, exptBfit, exptCfit, exptDfit), aic.txt)
expt.aic

summary(exptAfit, fit.measures=TRUE, standardized=T,rsquare=T)
standardizedSolution(survAfit)

### Fit multi-group analysis for the saturated model only

mod.AFit <- sem(model=mod.A, data=surv_expt, estimator='MLM', group = 'study') # no constraints

mod.AFit.equalreg <- sem(model=mod.A, data=surv_expt, estimator='MLM', group = 'study', group.equal = c("regressions")) # regression constraints

multi.aic <- aictab.lavaan(list(mod.AFit, mod.AFit.equalreg), c("No constraints", "Regression constraints"))

multi.aic # delta AICc is 1.44

### get fitted values for all three datasets
survAfit.val <- fitted.values(survAfit)
survAfit.val
write.csv(survAfit.val, here(folder, "data_output","survAfit.val.csv"))

exptAfit.val <- fitted.values(exptAfit)
write.csv(exptAfit.val, here(folder, "data_output","exptAfit.val.csv"))


mod.AFit.equalreg.val <- fitted.values(mod.AFit.equalreg)
write.csv(mod.AFit.equalreg.val, here(folder, "data_output","multi.val.csv"))

### MEPS appendix

cov.names <- c("space.logit", "prey.rich", "chiton.ln", "urchin.ln")

# Survey covariance matrix
survey.lower <- "1.780,
				-1.855, 12.784, 
				1.254, -1.488, 2.032
				0.567, -1.091, 0.521, 0.639"
survey.cov <- getCov(survey.lower, names=cov.names)
survey.cov
survey.means <- "-1.544, 12.222, 1.368, -1.133"
survey.N <- 72

mod.A <- '
# regressions
space.logit ~ urchin.ln
prey.rich ~ urchin.ln
space.logit ~ chiton.ln
prey.rich ~ chiton.ln
chiton.ln ~ urchin.ln
# covariances
space.logit ~~ prey.rich
' # end model

mod1 <- 'space.logit ~ urchin.ln'
mod1fit <- sem(mod1, sample.cov=survey.cov, sample.mean=survey.means, sample.nobs=survey.N)
surv.covA <- sem(mod.A, sample.cov=survey.cov, sample.mean=survey.means, sample.nobs=survey.N, estimator="MLM")




