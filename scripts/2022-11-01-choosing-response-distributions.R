#' if needed install the `faraway` package: `install.packages('faraway')`
library('mgcv')    # for Generalized Additive Models (GAMs) and more families
library('faraway') # for datasets
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
  labs(x = 'Aflatoxin B1 (ppb)', y = 'Relative frequency of tumors')

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

aflatoxin <- mutate(aflatoxin, freq = tumor / total)
aflatoxin

mutate(faraway::aflatoxin,
       p = tumor / total, # (0, 1)
       q = 1 - p, # (0, 1)
       odds = p / q, # (0, Inf)
       logit = log(odds)) # (-Inf, Inf)

m_afl <- gam(freq ~ dose,
             family = betar(link = 'logit'),
             data = aflatoxin)
## warning is ok, occurs because freq = 0 when dose = 0 and freq = 1 when dose = 100

plot(m_afl, all.terms = TRUE)
summary(m_afl)

## increase in risk of developing tumor as dose goes from 0 ppb to 10 ppb
ilogit(-3.70951 + 0.09141 * 0)
ilogit(-3.70951 + 0.09141 * 10)
ilogit(-3.70951 + 0.09141 * 75)

## plot the model
newd <- tibble(dose = seq(from = 0, to = 100, by = 1))
plot(newd$dose, predict(m_afl, newdata = newd, type = 'response'), type = 'l',
     xlab = 'Dose of Aflatoxin (ppb)', ylab = 'P(liver tumor)')
points(freq ~ dose, aflatoxin)

# also works for categories: alfalfa yield
?alfalfa

unique(alfalfa$irrigation) # factor, not a continuous variable
m_alf <- gam(yield ~ irrigation,
             family = Gamma(link = 'log'),
             data = alfalfa)

newd <- tibble(irrigation = unique(alfalfa$irrigation))
plot(newd$irrigation, predict(m_alf, newdata = newd, type = 'response'),
     xlab = 'Irrigation level', ylab = 'Yield')
points(yield ~ irrigation, alfalfa, pch = 19)

# repeat the plot but plot the data first
plot(formula = yield ~ irrigation, data = alfalfa,
     xlab = 'Irrigation level', ylab = 'Yield')
points(newd$irrigation, predict(m_alf, newdata = newd, type = 'response'),
       col = 'red', pch = 19)
