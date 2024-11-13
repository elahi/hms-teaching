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
library(tidyverse)

#### RULES ####

set.seed(111)
data <- data.frame(y1 = runif(100))
data$x1 <- data$y1 + runif(100)
unstd.model <- lm(y1 ~ x1, data)

data
data$y2 <- data$y1 + runif(100)
d <- as_tibble(data)
d

pairs(d)

residsx1 <- residuals(lm(x1 ~ y1, as.data.frame(apply(data, 2, scale))))
residsx1
plot(residsx1 ~ d$x1)

m2 <- lm(scale(data$y2) ~ residsx1)
summary(m2)
plot(m2)
m2$coefficients
summary(lm(scale(data$y2) ~ residsx1))$coefficients[2, 1]


# get unstandardized coefficient
summary(unstd.model)$coefficients[2, 1]

# Multiplying partial of x1 on y2 by y1 on y2
-0.01754746 * 0.6703465

# Subtracting standardized coef of x1-->y2 vs pathx1_y1 * pathy1_y2
0.4579473 - 0.4484743 # not the same

#### SEM ####

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

#### ####

x <- c(1, 2, 3, 4)
y <- c(2, 3, 4, 5)
y2 <- -(y)

cov(x, y)
cov(x, y2)
