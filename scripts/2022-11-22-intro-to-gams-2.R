#' if needed install the `faraway` package: `install.packages('faraway')`
library('mgcv')    # for Generalized Additive Models (GAMs) and more families
library('faraway') # for datasets
library('ggplot2') # for fancy plots
library('dplyr')   # for data wrangling
theme_set(theme_bw())

# simulated Normalized Difference Vegetation Index ("greenness") ----
source('https://github.com/StefanoMezzini/misc/raw/main/simulated-ndvi-data.R')
head(ndvi_data)

ggplot(ndvi_data, aes(date, ndvi)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

ggplot(ndvi_data, aes(doy, ndvi)) +
  geom_point(alpha = 0.3) +
  geom_smooth()

# start with a LM
lm_ndvi <- gam(ndvi ~ date,
               family = gaussian(),
               data = ndvi_data)

# predict from the model
ndvi_data$lm <- predict(lm_ndvi)

# LM fits the data badly 
ggplot(ndvi_data) +
  geom_point(aes(date, ndvi)) +
  geom_line(aes(date, lm, group = year), color = 'red', linewidth = 1)

# fit a GLM instead
# convert NDVI from (-1, 1) to (0, 1)
ndvi_data <- mutate(ndvi_data, ndvi_01 = (ndvi + 1) / 2,
                    dec_date = lubridate::decimal_date(date))

glm_ndvi <-gam(ndvi_01 ~ dec_date,
               family = betar(link = 'logit'),
               data = ndvi_data)

# predict from the GLM
ndvi_data <- mutate(ndvi_data,
                    eta = predict(glm_ndvi),
                    mu = glm_ndvi$family$linkinv(eta), # mu = g^-1(eta)
                    glm = mu * 2 - 1) # estimated NDVI from the GLM

# LM fits the data badly 
ggplot(ndvi_data) +
  geom_point(aes(date, ndvi)) +
  geom_line(aes(date, lm, group = year), color = 'red', linewidth = 1) +
  geom_line(aes(date, glm, group = year), color = 'darkorange', linewidth = 1)

# fit a GAM
gam_ndvi <-
  gam(ndvi_01 ~ s(dec_date),
      family = betar(link = 'logit'),
      data = ndvi_data,
      method = 'REML') # REsidual Maximum Likelihood

plot(gam_ndvi, scheme = 1, residuals = TRUE, all.terms = TRUE, pages = 1,
     trans = m_ndvi$family$linkinv)

# predict using the GAM
ndvi_data <- mutate(ndvi_data,
                    gam = glm_ndvi$family$linkinv(predict(gam_ndvi))*2-1)

# LM fits the data badly 
ggplot(ndvi_data) +
  geom_point(aes(date, ndvi)) +
  geom_line(aes(date, lm, group = year), color = 'red', linewidth = 1) +
  geom_line(aes(date, glm, group = year), color = 'darkorange', linewidth = 1) +
  geom_line(aes(date, gam, group = year), color = 'darkblue', linewidth = 1)

# variance isn't constant
plot(ndvi_data$date, residuals(m_ndvi), pch = 19, cex = 0.5,
     col = '#00000040') # black with some alpha

# estimated yearly temperature during the last millennium ----
?globwarm
head(globwarm)

m_temp <- gam(jasper ~ s(year), # using default k
              family = gaussian(),
              data = globwarm,
              method = 'REML')
plot(m_temp, scheme = 1, residuals = TRUE, cex = 2)

# using a higher k to allow the term to be more wiggly
m_temp <- gam(jasper ~ s(year, k = 30),
              family = gaussian(),
              data = globwarm,
              method = 'REML')
plot(m_temp, scheme = 1, residuals = TRUE, cex = 2)

# super wiggly!
gam(jasper ~ s(year, k = 300),
              family = gaussian(),
              data = globwarm,
              method = 'REML',
    control = gam.control(4)) %>%
  plot()

# variance isn't constant
plot(globwarm$year, residuals(m_temp), pch = 19, cex = 0.5,
     col = '#00000040') # black with some alpha

# when variance isn't constant, use a location-scale model ----
# simulated NDVI ----
m_ndvi_ls <-
  gam(list(ndvi ~ factor(year) + s(doy, bs = 'cc'),
           ~ s(doy, bs = 'cc')),
      family = gaulss(),
      data = ndvi_data,
      method = 'REML',
      knots = list(doy = c(0, 366))) # make 0 and 366 be the same day

plot(m_ndvi_ls, scheme = 1, all.terms = FALSE, pages = 1)

# temperature data ----
m_temp_ls <-
  gam(list(jasper ~ s(year, k = 30),
           ~ s(year, k = 10)),
      family = gaulss(),
      data = globwarm,
      method = 'REML',
      knots = list(doy = c(0, 366))) # make 0 and 366 be the same day

# can't convert to response scale easily because there are 2 link functions
plot(m_temp_ls, scheme = 1, all.terms = FALSE, pages = 1, n = 400)

# choose k carefully!
gam(list(jasper ~ s(year, k = 100),
         ~ s(year, k = 10)),
    family = gaulss(),
    data = globwarm,
    method = 'REML',
    knots = list(doy = c(0, 366))) %>%
  plot(scheme = 1, all.terms = FALSE, pages = 1, n = 400)

gam(list(jasper ~ s(year, k = 5),
         ~ s(year, k = 10)),
    family = gaulss(),
    data = globwarm,
    method = 'REML',
    knots = list(doy = c(0, 366))) %>%
  plot(scheme = 1, all.terms = FALSE, pages = 1)
