#' if needed install the `faraway` package: `install.packages('faraway')`
library('mgcv') # for Generalized Additive Models (GAMs) and more families
library('faraway') # for datasets
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('dplyr')   # for data wrangling
theme_set(theme_bw())

aflatoxin

# number of animals with liver cancer increases with toxin concentration
ggplot(aflatoxin) +
  geom_point(aes(dose, tumor))

# but total number of animals also increases, so there could be bias
ggplot(aflatoxin) +
  geom_point(aes(dose, total))

# standardize as probability of developing liver cancer
ggplot(aflatoxin) +
  geom_point(aes(dose, tumor / total)) +
  labs(x = 'Aflatoxin B1 (ppb)', y = 'P(tumor)')

# a linear model is inappropriate because:
# - it assumes P(tumor) can be below 0 or above 1
# - it does not allow any non-linearity
ggplot(aflatoxin, aes(dose, tumor / total)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  labs(x = 'Aflatoxin B1 (ppb)', y = 'P(tumor)')

# LMs (including all kinds of ANOVA) are not appropriate for most bounded
# data, including:
# - counts
# - proportions
# - concentrations
# - mass
# - NDVI
# - ...

# Examples ----
# Aflatoxin B1
?aflatoxin

# alfalfa yield
?alfalfa

# when terms are nonlinear, use a GAM ----
# diabetes and obesity
?diabetes # see "Details" section
## fit a model using chol, age, gender, and weight as predictors

gam(glyhb ~ s(chol) + s(age) + gender + s(weight),
    family = Gamma('log'),
    data = diabetes) %>%
  plot(all.terms = TRUE, pages = 1)

## model glyhb

## model a binary variable of diabetic or not

# estimated yearly temperature during the last millennium
?globwarm

# simulated Normalized Difference Vegetation Index (habitat "greenness")
source('https://github.com/StefanoMezzini/misc/raw/main/simulated-ndvi-data.R')
head(ndvi_data)

# when variance isn't constant, use a location-scale model ----
# temperature data
globwarm

# simulated NDVI
ndvi_data
