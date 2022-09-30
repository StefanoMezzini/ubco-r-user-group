library('ggplot2')        # for fancy figures
library('dplyr')          # for data wrangling
library('palmerpenguins') # for penguin data

# create a scatterplot
ggplot(data = penguins) +
  geom_point(mapping = aes(bill_length_mm, bill_depth_mm))

# points + lines, with a single color for each layer 
ggplot(data = penguins, mapping = aes(bill_length_mm, bill_depth_mm)) +
  geom_point(color = 'darkorange') +
  geom_line(color = 'forestgreen')

# color points by species
p <-
  ggplot(penguins) +
  geom_point(aes(bill_length_mm, bill_depth_mm, color = species))
p

# add better labels
p <- p + labs(x = 'Bill length (mm)', y = 'Bill depth (mm)')
p

# use a different color palette
p <- p + scale_color_brewer('Species', type = 'qual', palette = 1)
p

# use a different theme
p + theme_bw()
p + theme_light()
p + theme_classic()
p + theme_gray()
p + theme_linedraw()
p + theme_minimal()
p + theme_void()

# change the default ggplot theme to use a theme without having to add it
theme_set(theme_bw())
p

# add a regression line (not ideal for modeling, but ok for exploratory stuff)
ggplot(penguins, aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point() + # using common aesthetics defined in ggplot()
  geom_smooth(aes(fill = species), method = 'lm', formula = y ~ x) + # add fill aesthetic
  scale_color_brewer('Species', type = 'qual', palette = 1,
                     aesthetics = c('color', 'fill'))

# add a GLM instead of a LM
ggplot(penguins, aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point() +
  # add LMs to compare, but with no color --> don't inherit aes from ggplot()
  geom_smooth(aes(bill_length_mm, bill_depth_mm, group = species),
              inherit.aes = FALSE, method = 'lm', formula = y ~ x,
              color = 'grey25', se = FALSE) +
  # add GLMs with color and fill
  geom_smooth(aes(fill = species), method = 'glm', formula = y ~ x,
              method.args = list(family = Gamma(link = 'log'))) +
  scale_color_brewer('Species', type = 'qual', palette = 1,
                     aesthetics = c('color', 'fill'))

# create a histogram
ggplot(penguins, aes(flipper_length_mm)) +
  geom_histogram(fill = 'grey80', color = 'black')

# create histograms split by species
ggplot(penguins, aes(flipper_length_mm, fill = species)) +
  geom_histogram(color = 'black', alpha = 0.5) +
  scale_fill_viridis_d('Species', option = 'viridis')

# create separate facets for each species
ggplot(penguins, aes(flipper_length_mm, fill = species)) +
  facet_wrap(~ species, scales = 'free_y', ncol = 1) + # 1-sided formula
  geom_histogram(color = 'black', alpha = 0.5) +
  scale_fill_viridis_d('Species', option = 'viridis')

# create separate facets for each species and sex, in a grid
ggplot(penguins, aes(flipper_length_mm, fill = species)) +
  facet_grid(sex ~ species, scales = 'free_y') + # 2-sided formula (check axes)
  geom_histogram(color = 'black', alpha = 0.5, binwidth = 10) + # wider bin width
  scale_fill_viridis_d('Species', option = 'viridis')

# create density plots of flipper length
ggplot(penguins, aes(flipper_length_mm, fill = species)) +
  facet_grid(sex ~ species, scales = 'free_y') +
  geom_density(color = 'black', alpha = 0.5, bw = 2) + # binwidth is sensitive
  scale_fill_viridis_d('Species', option = 'viridis')

# create a boxplot of the flipper length 
ggplot(penguins, aes(x = sex, y = flipper_length_mm)) +
  facet_grid(species ~ ., scales = 'free_y') +
  geom_boxplot(fill = 'grey')

ggplot(penguins, aes(y = flipper_length_mm)) +
  facet_grid(sex ~ species, scales = 'free_y') +
  geom_boxplot(fill = 'grey') +
  scale_x_continuous(breaks = NULL)

# create a bar plot of average bill depth with error bars
p1 <-
  penguins %>%
  filter(! is.na(sex)) %>% # remove NAs
  group_by(species, sex) %>% # calculate stats for each species and sex
  summarize(depth = mean(bill_depth_mm)) %>% # calculate mean bill depth
  mutate(lower_ci = depth - 2, # add fictitious CIs as an aexample
         upper_ci = depth + 2) %>%
  ggplot() +
  facet_grid(species ~ .) +
  geom_col(aes(sex, depth, fill = sex), width = 0.5, show.legend = FALSE) +
  geom_errorbar(aes(sex, ymin = lower_ci, ymax = upper_ci), width = 0.5)
p1

# use a log10 y axis
p1 + scale_y_log10()

# reverse the y axis
p1 + scale_y_reverse()
