#' if needed install the `faraway` package: `install.packages('faraway')`
library('mgcv')    # for Generalized Additive Models (GAMs) and more families
library('faraway') # for datasets
library('tidyr')   # for pivot_*() functions
library('dplyr')   # for mutate and %>%
library('ggplot2') # for fancy plots

# modeling temperature in Jasper
head(globwarm)
?globwarm
m_temp <- gam(jasper ~ s(year, k = 30),
              family = gaussian(),
              data = globwarm,
              method = 'REML')
plot(m_temp, scheme = 1, residuals = TRUE, cex = 2)

# modeling all the temperatures in all locations
globwarm_long <- pivot_longer(globwarm,
                              cols = - c(year),
                              names_to = 'location',
                              values_to = 'temp_C',
                              values_drop_na = TRUE)

# not using factors causes an error
gam(temp_C ~ s(year, location, bs = 'fs'),
    family = gaussian(),
    data = globwarm_long,
    method = 'REML')

# change locations to factors
globwarm_long <- mutate(globwarm_long, location = factor(location))

# find number of cores (not logical processors)
NCORES <- parallel::detectCores(logical = FALSE)
NCORES

# using similar smoothness with bs = 'fs'
m_temp <- gam(temp_C ~ s(year, location, bs = 'fs', k = 30),
              family = gaussian(),
              data = globwarm_long,
              method = 'REML',
              control = gam.control(nthreads = NCORES)) # parallelize
plot(m_temp, scheme = 0, residuals = TRUE, n = 400)
