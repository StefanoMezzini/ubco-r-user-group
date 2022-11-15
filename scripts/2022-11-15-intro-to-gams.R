#' if needed install the `faraway` package: `install.packages('faraway')`
library('mgcv')    # for Generalized Additive Models (GAMs) and more families
library('faraway') # for datasets
library('ggplot2') # for fancy plots
library('cowplot') # for fancy multi-panel plots
library('dplyr')   # for data wrangling
theme_set(theme_bw())

# when terms are nonlinear, use a GAM ----
# diabetes and obesity using ... ----
?diabetes # see "Details" section

# ... chol, age, gender, and weight as predictors and...
## ... glyhb as the response ----
m_diab_1 <- gam(glyhb ~ s(chol) + s(age) + gender + s(weight),
                family = 'choose a family',
                data = diabetes,
                method = 'REML') # REstricted Maximum Likelihood

# plotting the GAM with base plot()
plot(m_diab_1,
     all.terms = TRUE, # include parametric gender term
     pages = 1,        # all plots in one page
     scheme = 1,       # use shaded CIs instead of dashed lines
     trans = exp)      # return to response scale, i.e. (0, Inf)

## ... a binary variable of diabetic or not as the response ----
diabetes <- mutate(diabetes, diabetic = glyhb > 7)

ggplot(diabetes) +
  geom_histogram(aes(glyhb, fill = diabetic), bins = 12, color = 1) +
  geom_vline(xintercept = 7, color = 2) +
  scale_fill_brewer('Diabetic', type = 'qual', palette = 6,
                    labels = c('No', 'Yes'), direction = -1)

m_diab_2 <- gam(diabetic ~ s(chol) + s(age) + gender + s(weight),
                family = 'choose a family',
                data = diabetes,
                method = 'REML')

plot(m_diab_2, all.terms = TRUE, pages = 1, scheme = 1,
     trans = m_diab_2$family$linkinv)

# simulated Normalized Difference Vegetation Index ("greenness") ----
source('https://github.com/StefanoMezzini/misc/raw/main/simulated-ndvi-data.R')
head(ndvi_data)

ggplot(ndvi_data, aes(date, ndvi)) +
  geom_point(alpha = 0.3)

# convert NDVI from (-1, 1) to (0, 1)
ndvi_data <- mutate(ndvi_data, ndvi_01 = (ndvi + 1) / 2)
m_ndvi <-
  gam(ndvi_01 ~ factor(year) + s(doy, bs = 'cc'),
      family = 'choose a family',
      data = ndvi_data,
      method = 'REML',
      knots = list(doy = c(0, 366))) # make 0 and 366 be the same day
plot(m_ndvi, scheme = 1, residuals = TRUE, all.terms = TRUE, pages = 1,
     trans = m_ndvi$family$linkinv)

# variance isn't constant
plot(ndvi_data$date, residuals(m_ndvi), pch = 19, cex = 0.5,
     col = '#00000040') # black with some alpha

# estimated yearly temperature during the last millennium ----
?globwarm
head(globwarm)

m_temp <- gam(jasper ~ s(year), # using default k
              family = 'choose a family',
              data = globwarm,
              method = 'REML')
plot(m_temp, scheme = 1, residuals = TRUE)

# using a higher k to allow the term to be more wiggly
m_temp <- gam(jasper ~ s(year, k = 30),
              family = 'choose a family',
              data = globwarm,
              method = 'REML')
plot(m_temp, scheme = 1, residuals = TRUE)

# variance isn't constant
plot(globwarm$year, residuals(m_temp), pch = 19, cex = 0.5,
     col = '#00000040') # black with some alpha

# when variance isn't constant, use a location-scale model ----
# simulated NDVI ----
m_ndvi_ls <-
  gam(list(ndvi ~ factor(year) + s(doy, bs = 'cc'),
           ~ s(doy, bs = 'cc')),
      family = 'choose a family',
      data = ndvi_data,
      method = 'REML',
      knots = list(doy = c(0, 366))) # make 0 and 366 be the same day

plot(m_ndvi_ls, scheme = 1, all.terms = FALSE, pages = 1)

# temperature data ----
m_temp_ls <-
  gam(list(jasper ~ s(year, k = 30),
           ~ s(year, k = 10)),
      family = 'choose a family',
      data = globwarm,
      method = 'REML',
      knots = list(doy = c(0, 366))) # make 0 and 366 be the same day

# can't convert to response scale easily because there are 2 link functions
plot(m_temp_ls, scheme = 1, all.terms = FALSE, pages = 1)

# choose k carefully!
gam(list(jasper ~ s(year, k = 100),
           ~ s(year, k = 10)),
      family = 'choose a family',
      data = globwarm,
      method = 'REML',
      knots = list(doy = c(0, 366))) %>%
  plot(scheme = 1, all.terms = FALSE, pages = 1)

gam(list(jasper ~ s(year, k = 5),
         ~ s(year, k = 10)),
    family = 'choose a family',
    data = globwarm,
    method = 'REML',
    knots = list(doy = c(0, 366))) %>%
  plot(scheme = 1, all.terms = FALSE, pages = 1)
