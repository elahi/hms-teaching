##' Robin Elahi
##' https://jslefche.github.io/sem_book
##' Chapter 3
##' 9 Dec 2024

# install.packages("lavaan")
# install.packages("piecewiseSEM")
# devtools::install_github("jslefche/piecewiseSEM@devel")

library(here)
library(lavaan)
library(piecewiseSEM)
library(tidyverse)

#### 3.4 ####
data("keeley")

keeley_psem <- psem(
  lm(cover ~ firesev, data = keeley),
  lm(firesev ~ age, data = keeley),
  data = keeley)

keeley_psem

basisSet(keeley_psem)

# test the claim with dsep test
dSep(keeley_psem, .progressBar = FALSE)

summary(lm(cover ~ firesev + age, data = keeley))$coefficients[3, ]

# Fisher's c-statistic
C <- -2 * log(summary(lm(cover ~ firesev + age, data = keeley))$coefficients[3, 4])
-2 * log(0.07) # based on the p-value of the independence claim(s)
C
1-pchisq(C, 2)
fisherC(keeley_psem)

# LogLik approach
keeley_psem2 <- psem(
  lm(cover ~ firesev + age, data = keeley),
  lm(firesev ~ age, data = keeley),
  data = keeley
)

LL_1 <- logLik(lm(cover ~ firesev, data = keeley)) - 
  logLik(lm(cover ~ firesev + age, data = keeley))

LL_2 <- logLik(lm(firesev ~ age, data = keeley)) - 
  logLik(lm(firesev ~ age, data = keeley))

(ChiSq <- -2*sum(as.numeric(LL_1), as.numeric(LL_2)))

DF <- 1 # one additional parameter estimated in the saturated model

1 - pchisq(ChiSq, DF)

LLchisq(keeley_psem)

# Lovaan inference is same; because we assumed multivariate normality w psem
keeley_formula <- '
firesev ~ age
cover ~ firesev
'

keeley_sem <- sem(keeley_formula, data = keeley)

fit <- lavInspect(keeley_sem, "fit")

fit["chisq"]; fit["pvalue"]

AIC(keeley_psem)
AIC(keeley_psem, AIC.type = "dsep")

# Easy psem_easy
summary(keeley_psem, .progressBar = FALSE)
summary(keeley_psem2, .progressBar = FALSE)

# Compare with lavaan
sem1 <- '
firesev ~ age
cover ~ firesev
'

keeley_sem1 <- sem(sem1, keeley)

summary(keeley_sem1, standardize = T, rsq = T)

#### 3.5 ####

# Start w lavaan
data(shipley)

shipley_model <- '
DD ~ lat
Date ~ DD
Growth ~ Date
Live ~ Growth
'

shipley_sem <- sem(shipley_model, shipley)

summary(shipley_sem, standardize = T, rsq = T)

# Model does not recreate the vcov structure (p < 0.001); try piecewise

library(nlme)
library(lme4)

shipley_psem <- psem(
  
  lme(DD ~ lat, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  
  lme(Date ~ DD, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  
  lme(Growth ~ Date, random = ~ 1 | site / tree, na.action = na.omit,
      data = shipley),
  
  glmer(Live ~ Growth + (1 | site) + (1 | tree),
        family = binomial(link = "logit"), data = shipley)
  
)

summary(shipley_psem, .progressBar = FALSE)

#### 3.6 ####

# simulate data
set.seed(100)
n <- 100
x1 <- rchisq(n, 7)
mu2 <- 10*x1/(5 + x1)
x2 <- rnorm(n, mu2, 1)
x2[x2 <= 0] <- 0.1
x3 <- rpois(n, lambda = (0.5*x2))
x4 <- rpois(n, lambda = (0.5*x2))
p.x5 <- exp(-0.5*x3 + 0.5*x4)/(1 + exp(-0.5*x3 + 0.5*x4))
x5 <- rbinom(n, size = 1, prob = p.x5)
dat2 <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)

# assume normality
shipley_psem2 <- psem(
  lm(x2 ~ x1, data = dat2),
  lm(x3 ~ x2, data = dat2),
  lm(x4 ~ x2, data = dat2),
  lm(x5 ~ x3 + x4, data = dat2)
)

LLchisq(shipley_psem2)
summary(shipley_psem2)

# mix of glms and gams
library(mgcv)

shipley_psem3 <- psem(
  gam(x2 ~ s(x1), data = dat2, family = gaussian),
  glm(x3 ~ x2, data = dat2, family = poisson),
  gam(x4 ~ x2, data = dat2, family = poisson),
  glm(x5 ~ x3 + x4, data = dat2, family = binomial)
)

LLchisq(shipley_psem3) # does not fit!

AIC(shipley_psem2, shipley_psem3)

summary(shipley_psem3)

#### 3.7 ####

set.seed(87)

glmdat <- data.frame(x1 = runif(50), y1 = rpois(50, 10), 
                     y2 = rpois(50, 50), y3 = runif(50))

# LM
summary(lm(y1 ~ y2 + x1, glmdat))$coefficients[2, 4]

summary(lm(y2 ~ y1 + x1, glmdat))$coefficients[2, 4]

# GLM
summary(glm(y1 ~ y2 + x1, "poisson", glmdat))$coefficients[2, 4] 

summary(glm(y2 ~ y1 + x1, "poisson", glmdat))$coefficients[2, 4]

# LogLiks are different for glm also
logLik(glm(y1 ~ y2 + x1, "poisson", glmdat))
logLik(glm(y2 ~ y1 + x1, "poisson", glmdat))

# psem addresses by default
glmsem <- psem(
  glm(y1 ~ x1, "poisson", glmdat),
  glm(y2 ~ x1, "poisson", glmdat),
  lm(y3 ~ y1 + y2, glmdat)
)

summary(glmsem)

## Option 1
summary(glmsem, direction = c("y1 <- y2"), .progressBar = F)$dTable

## Option 2
summary(update(glmsem, y1 %~~% y2), .progressBar = F)

## Option 3
summary(glmsem, conserve = T, .progressBar = F)$dTable

