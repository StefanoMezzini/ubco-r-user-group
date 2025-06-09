#' if needed install the `faraway` package: `install.packages('faraway')`
library('mgcv')    # for Generalized Additive Models (GAMs) and more families
library('faraway') # for datasets
library('ggplot2') # for fancy plots
library('dplyr')   # for data wrangling
theme_set(theme_bw())

?mgcv::family.mgcv

# when terms are nonlinear, use a GAM ----
# using chol, age, gender, and weight as predictors and... ----
?diabetes # see "Details" section
range(diabetes$glyhb, na.rm = TRUE)

## ... glyhb as the response ----
m_diab_1 <- gam(glyhb ~ s(chol) + s(age) + gender + s(weight),
                family = Gamma(link = 'log'),
                data = diabetes,
                method = 'REML') # REstricted Maximum Likelihood

# plotting the GAM with base plot()
plot(m_diab_1,
     all.terms = TRUE, # include parametric gender term
     pages = 1,        # all plots in one page
     scheme = 1)       # use shaded CIs instead of dashed lines

plot(m_diab_1,
     all.terms = TRUE, # include parametric gender term
     pages = 1,        # all plots in one page
     scheme = 1,       # use shaded CIs instead of dashed lines
     trans = exp)      # return to response scale, i.e. (0, Inf)

## ... a binary variable of diabetic or not as the response ----
diabetes <- mutate(diabetes, diabetic = glyhb > 7)

ggplot(data = diabetes) +
  geom_histogram(aes(glyhb, fill = diabetic), bins = 12, color = 1) +
  geom_vline(xintercept = 7, color = 2) +
  scale_fill_brewer('Diabetic', type = 'qual', palette = 6,
                    labels = c('No', 'Yes'), direction = -1)

m_diab_2 <- gam(diabetic ~ s(chol) + s(age) + gender + s(weight),
                family = binomial(link = 'logit'),
                data = diabetes,
                method = 'REML')

plot(m_diab_2, all.terms = TRUE, pages = 1, scheme = 1,
     trans = m_diab_2$family$linkinv)
