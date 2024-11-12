##' Robin Elahi
##' https://jslefche.github.io/sem_book
##' Chapter 2
##' 12 Nov 2024

# install.packages("lavaan")
# install.packages("piecewiseSEM")
# devtools::install_github("jslefche/piecewiseSEM@devel")

library(here)
library(lavaan)
library(piecewiseSEM)

data(keeley)
head(keeley)

keeley_formula1 <- 'firesev ~ age'
class(keeley_formula1)

keeley_sem1 <- sem(keeley_formula1, data = keeley)

summary(keeley_sem1)

keeley_mod <- lm(firesev ~ age, data = keeley)
summary(keeley_mod)$coefficients

summary(sem(keeley_formula1, keeley, meanstructure = T))

cov(keeley[, c("firesev", "age")])[2, 1]/var(keeley$age)

cor(keeley$firesev, keeley$age)

coefs(keeley_mod)

standardizedsolution(keeley_sem1)

summary(keeley_sem1, standardize = T)

#### 2.6.2 ####

keeley_formula2 <- '
firesev ~ age
cover ~ firesev
'

keeley_sem2 <- sem(keeley_formula2, data = keeley)

summary(keeley_sem2, standardize = T, rsq = T)

fitMeasures(keeley_sem2)


keeley_formula2.1 <- '
firesev ~ B1 * age
cover ~ B2 * firesev

indirect := B1 * B2
'

keeley_sem2.1 <- sem(keeley_formula2.1, keeley)

summary(keeley_sem2.1, standardize = T)

#### Model 3 ####
keeley_formula3 <- '
firesev ~ age
cover ~ firesev + age
'

keeley_sem3 <- sem(keeley_formula3, data = keeley)

# This model is saturated, can't calculate goodness of fit
summary(keeley_sem3, standardize = T)

anova(keeley_sem2, keeley_sem3)
