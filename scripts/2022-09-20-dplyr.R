library('dplyr') # for data wrangling
library('palmerpenguins') # for penguin data

summary(penguins) # look at a summarized version of the dataset
View(penguins) # look at the full dataset
class(penguins) # "tibbles" are special data frames (see ?tibble::tibble)

# pull a column out of a dataset (as a vector or list)
penguins$species # base R way
pull(penguins, species) # "tidy" way

#' relocate a column within `penguins`
relocate(.data = penguins, island, .before = species)
relocate(penguins, year, .before = 1)
relocate(penguins, year, .before = 3)
relocate(penguins, year, .after = 3)
relocate(penguins, island, .after = last_col())

# add or change some columns (at the end of the data.frame)
mutate(penguins,
       sex = stringr::str_to_title(sex), # capitalize first initials
       year = as.numeric(year), # change year from integer to decimal
       bill_length_depth_ratio = bill_length_mm / bill_depth_mm,
       flipper_length_mass_ratio = bill_length_mm / body_mass_g)

# create new columns, drop the old ones
transmute(penguins,
          species, # keep the species column (unchanged)
          island, # keep the island column (unchanged)
          flipper_length_m = flipper_length_mm / 1e3,
          big = body_mass_g > 4500,
          flipper = if_else(condition = flipper_length_mm > 200,
                            true = 'long',
                            false = 'short'),
          bill = case_when(bill_length_mm < 40 ~ 'short',
                           bill_length_mm < 50 ~ 'medium',
                           bill_length_mm < 60 ~ 'long',
                           is.na(bill_length_mm) ~ 'missing'))

# change entire groups of columns (also works with mutate())
transmute(penguins,
          across(.cols = c(species, island, sex), .fns = toupper),
          across(.cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm),
                 .fns = round))

# change the name of some columns
rename(penguins,
       Species = species,
       Location = island)

# select only some of the columns
select(penguins, species, island, body_mass_g)
select(penguins, c(species, island, body_mass_g)) # can also use c()

# select all columns except for some
select(penguins, -species, -island, -body_mass_g)
select(penguins, -c(species, island, body_mass_g)) # c() avoids having to repeat "-"

# filter to only some penguins (similar to subset() from base R)
filter(penguins, species == 'Adelie') # only Adelie penguins
filter(penguins, island != 'Torgersen') # only penguins not from Torgersen
filter(penguins, sex == 'female', body_mass_g > 5000) # heavier AND female penguins
filter(penguins, sex == 'female' & body_mass_g > 5000) # as above, but using "&"
filter(penguins, sex == 'female' | body_mass_g > 5000) # female OR heavier penguins
filter(penguins, # (1 AND 2 AND 3) OR (4 AND 5 AND 6)
       (year == 2007 & species == 'Adelie' & sex == 'male') |
         (year == 2009 & island == 'Biscoe' & flipper_length_mm < 185))

# arrange the table by island (alphabetical/ascending order)
arrange(penguins, island)

# arrange the table by year (descending order)
arrange(penguins, desc(year))

# arrange the table by year (descending order), then sex (descending order)
arrange(penguins, desc(year), desc(sex))

# bind rows together
bind_rows(penguins[1:3, ], # first 3 rows (base R)
          slice(penguins, 20:22)) # rows 20-22 (tidy way)

#' `bind_rows()` places `NA`s if a column is missing
bind_rows(penguins[1:3, -1], # first 3 rows, drop first column (base R)
          slice(penguins, 20:22)) # rows 20-22 (tidy way)

# bind columns together
bind_cols(penguins[ , c('species', 'island')],
          select(penguins, sex, body_mass_g))

# summarize the dataset
summarize(penguins,
          sample_size = n(), # similar to nrow() or length(), but more flexible
          mean_flipper_length_mm = mean(flipper_length_mm), # some NAs make mean NA
          median_body_mass_g = median(body_mass_g, na.rm = TRUE), # remove NAs
          max_year = max(year),
          n_species = n_distinct(species),
          n_sexes = n_distinct(sex))

#' *note*: you can use `%>%` (called a "pipe") to pass the result to the next function
#' via a "pipeline" of consecutive functions
# summarize by groups
penguins %>%
  filter(! is.na(sex)) %>% # remove penguins with unknown sex
  group_by(species, sex) %>% # group by species and sex
  summarize(sample_size = n(),
            mean_flipper_length_mm = mean(flipper_length_mm),
            median_body_mass_g = median(body_mass_g, na.rm = TRUE),
            max_year = max(year))

# let's break things a bit...
d <- mutate(penguins, id = 1:n()) # add a column with unique IDs

d1 <- d %>%
  slice(1:10) %>% # take first 10 rows
  select(id, sex, species) %>% # only take some columns
  arrange(sex, species)

d2 <- d %>%
  slice(1:10) %>% # take first 10 rows
  select(id, flipper_length_mm, bill_length_mm, bill_depth_mm)

# this won't work (compare id columns)
bind_cols(d1, d2)

#' join by `id` (compare to `d`)
full_join(x = d1, y = d2, by = 'id') %>% # argument(s) for `by` need to be in quotations!
  arrange(id)

#' join a portion of `d1` to `d2`, but only keep rows from `d1` (the "left side")
slice(d1, 1:5) %>% # only first 5 rows
  left_join(d2, by = 'id') # note that the number of rows == nrow(d1) but < nrow(d2)

#' join a portion of `d1` to `d2`, but keep rows from `d2` (the "right side")
slice(d1, 1:5) %>% # only first 5 rows
  right_join(d2, by = 'id') # note missing data because nrow(d1) < nrow(d2)

#' join a portion of `d1` to a portion of `d2`, but only keep rows in both portions
inner_join(slice(d1, 1:5), slice(d2, 1:5), by = 'id')

#' join a portion of `d1` to a portion of `d2`, and keep ALL rows
full_join(slice(d1, 1:5), slice(d2, 1:5), by = 'id')
