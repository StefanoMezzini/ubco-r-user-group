#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#-#-#-#   Data Manipulation and Cleaning                            #-#-#-#
#-#-#-#   This tutorial was originally developed by:                #-#-#-# 
#-#-#-#   Diane Srivastava & Joey Burant (CIEE Living Data Project) #-#-#-#
#-#-#-#   Adapted by Elizabeth Houghton (2022-10-18)                #-#-#-#   
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

## Line 11 -> Data manipulation exercise
## Line 266 -> Data cleaning exercise

### PART 1: DATA MANUPULATION
### SET-UP ----------------------------------------------------------------

## Working Directory ##

## check if working directory is set to the correct current folder
getwd() 

# if it is not the correct folder, use setwd() to set it to the correct folder
# alternatively, in the panel with the "Files" tab, navigate to the correct folder,
# then click "More" (with the gear next to it), and select "Set As Working Directory"


## Packages and Data ##

### import (or install) required packages

## if you need to install the packages, run install.packages() with the package 
## name in quotes and then run library() with the package name in parentheses 
# (remove # to run install.packages())

# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("palmerpenguins")


library(tidyverse)
library(dplyr)
library(palmerpenguins) # for penguin data


## Import Files ##

## if we wanted to pull data from a file on our desktop instead of installing the 
## package palmerpenguins which has data in it, we could run:

# df <- read.csv(file='df.csv')

## this would read in the csv file 'df'. Just make sure your working directory
## is set to where this file is being stored on your computer


############################################################################### 
### The pipe: %>% ###
## keyboard shortcut: Ctrl+shift+m / cmd+shift+m

# the pipe %>% allows you to invoke multiple calls without needing to 
# store the intermediate results as variables.

## Example: lets look at the dataset 'penguins' which was included in the palmerpenguins
## package
head(penguins)

# lets pull this dataset into our environment and call it 'penguins'
data(penguins)

# lets check that that each variable is assigned the right data type

str(penguins)

# you can see that 'species', 'island', and 'sex' are factors,
# 'bill_length_mm' and 'bill_depth_mm' are numeric (num),
# and 'flipper_length_mm', 'body_mass_g', and 'year' are integers (int)

# sometimes your data will be assigned to the wrong data type when you pull it into R
# so it is always a good habit to double check early on. If you needed to assign your data
# coloumns to a different data type it would look something like this:

# e.g. assign the species column to character type
# penguins$species <- as.character(penguins$species)

# e.g. assign the bill_length_mm column to integer
# penguins$bill_length_mm <- as.integer(penguins$bill_length_mm)

# other important commands: as.Date(), as.numeric(), as.factor()

## Let's calculate the average bill length (bill_length_mm) by species (species)

# for this tutorial, we will also remove any rows that have NA values in our dataset:
penguins <- na.omit(penguins)

# Option 1: Store each step in the process sequentially
# first group by species
species_int <- group_by(penguins, species)
species_int

# then calculate average bill length
result <- summarise(species_int, mean_bill_length_mm = mean(bill_length_mm))
result

# Option 2: use the pipe %>% to chain functions
result <- group_by(penguins, species) %>% 
  summarise(species_int, mean_bill_length_mm = mean(bill_length_mm))
result

# by using the pipe command, we can skip a step and calcuate this all in one go!


###############################################################################  
### DPLYR::SELECT ###
## select columns of interests that you want to work with

## first, let's look at the columns in our data frame again
head(penguins)

## to select columns by name:
penguins %>%
  select(species, sex, year)

## to assign the selected columns to a new data frame:
penguins_reduced1 <- penguins %>%
  select(species, sex, year)

head(penguins_reduced1)

## to select columns column number:
penguins %>%
  select(1, 7, 8)

## again, to assign the selected columns to a new data frame:

penguins_reduced2 <- penguins %>%
  select(1, 7, 8)

head(penguins_reduced2)


############################################################################### 
### DPLYR::SUBSET ###
## lets conditionally select rows from our data set based on column values
## lets select penguins that only weigh over 4000 g

penguins_heavy <- subset(penguins, body_mass_g > 4000)

## you can combine multiple columns based on boolean operator commands as well
# e.g  penguins that only weigh over 4000 g *AND* have a bill length greater than or equal to 40 mm

penguins_large <- subset(penguins, body_mass_g > 4000 & bill_length_mm >= 40)

# e.g  penguins that only weigh over 4000 g *OR* have a flipper length greater than 200 mm

penguins_big <- subset(penguins, body_mass_g > 4000 | flipper_length_mm > 200)


############################################################################### 
### DPLYR::RENAME ###

## we can rename a variable by using the select command
penguins_renamed <- penguins %>%
  select(spp = species, 
         island, 
         bill_length = bill_length_mm,
         body_mass_g,
         yr = year)
head(penguins_renamed)

# or by using DPLYR::RENAME
penguins_renamed <- penguins_renamed %>%
  dplyr::rename(body_mass = body_mass_g)
head(penguins_renamed)


############################################################################### 
### DPLYR::ARRANGE ###
## We can sort this data frame by year (or any other column if we wanted)
penguins_renamed <- penguins_renamed %>%
  arrange(yr)

penguins_renamed

## ARRANGE: descending
## We can also reverse the order
penguins_renamed <- penguins_renamed %>%
  arrange(desc(yr))


############################################################################### 
### DPLYR::COUNT ###
## Count the number of penguins for each species in our full data frame
penguins %>%
  count(species)

## sort from most to least common
penguins %>%
  count(species) %>%
  arrange(desc(n))

## or you can use
penguins %>%
  count(species, sort = TRUE)


###############################################################################
### DPLYR::MUTATE ###

## If we wanted to create a new column based on calculations from a column in our
## data frame we could use the mutate() function. For example, if we wanted to find
## the ratio of bill length to bill mass we could calculate it and assign it to the 
## new column 'bill_length_depth' as follows:

penguins <- penguins %>%
  mutate(bill_length_depth = bill_length_mm / bill_depth_mm)


### mutate() combined with ifelse()
## If we wanted to categorize penguins based on their weight
## small: < 3900 g
## medium: 3900 - 5100 g
## large: > 5100 g
penguins %>%
  mutate(penguins_size = ifelse(body_mass_g < 3900, "small",
                                 ifelse(body_mass_g <= 5100, "medium",
                                        ifelse(body_mass_g > 5100, "large", NA))))

## let's visualize the split between size categories by piping this into count and ggplot
penguins %>%
  mutate(penguins_size = ifelse(body_mass_g < 3900, "small",
                                ifelse(body_mass_g <= 5100, "medium",
                                       ifelse(body_mass_g > 5100, "large", NA)))) %>% 
  count(penguins_size) %>%
  ggplot(aes(x=penguins_size, y=n)) +
  geom_bar(stat = "identity")

# we can see there is many more medium and small penguins than large penguins here



###############################################################################
### TIDYR::SEPARATE ###

# now lets look at the penguins_raw data frame
head(penguins_raw)

## What if you wanted to separate genus and species from this data frame?

# first I need to quickly pull out the text in brackets which contains the genus and species names

# separate rows on closing brackets
penguins_raw <- penguins_raw %>% separate(Species, c("Name", "Penguin", "Genus", "Species"), sep = " ") %>%
  # separate the column string into four new columns (Name, Penguin, Genus, Species) sperated by a space
               select(-Name, -Penguin) # remove Name and Penguin columns

head(penguins_raw)

# we see due to the data naming we need to remove the '(' before the Genus and the ')' after the species
# we can remove the first character in the Genus column using the substring() function
penguins_raw$Genus <- substring(penguins_raw$Genus, 2)

penguins_raw$Genus

# similarly, we can remove the last character in the Species column using the substring() function
penguins_raw$Species <- substring(penguins_raw$Species, 1, nchar(penguins_raw$Species) - 1)



### PART 2: DATA Cleaning
### SET-UP ---------------------------------------------------------------------

## check your working directory
getwd()

## install and call required packages

## if you need to install the packages, run install.packages() with the package 
## name in quotes and then run library() with the package name in parentheses 
# (remove # to run install.packages())

# install.packages("palmerpenguins")
# install.packages("taxize")
# install.packages("assertr")
# install.packages("stringdist")
# install.packages("tidyverse")
# install.packages("GGally")
# install.packages("ggplot2")

library("palmerpenguins")
library("taxize")
library("assertr")
library("stringdist")
library("tidyverse")
library("GGally")
library("ggplot2")

###############################################################################
### EXPLORE DATA ###

## let's explore the penguins dataset from the palmerpenguins package

## load data
data(penguins)

## some initial exploration steps
dim(penguins) # [1] 344 rows (observations) and 8 columns (variables)
head(penguins, 10) ## view the first 10 rows of data
summary(penguins) ## summarize the data
skimr::skim(penguins) ## more detailed data summary
str(penguins) ## we can look at the data types (factor, numeric, integer)

## plot the data
penguins %>%
  select(species, body_mass_g, ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species))

## add a column with Latin species names
## all 3 are members of the Pygoscelis genus of brush-tailed penguins
penguins <- penguins %>% 
  mutate(speciesL = case_when(species == "Adelie" ~ "adeliae", 
                              species == "Chinstrap" ~ "antarctica", 
                              species == "Gentoo" ~ "papua")) %>% 
  relocate(speciesL, .after = 1)

view(penguins)

###############################################################################
### IDENTIFYING DATA ERRORS ###
## Lets add some errors in the data frame so we can work through some strategies
## on how we might identify these possible mistakes

## introducing errors
penguins$speciesL[10] <- "adelaie" ## typo in species name

penguins <- penguins[-c(15, 100, 200, 273), ] ## delete some rows

penguins$flipper_length_mm[301] <- ## flip sign (-'ve value)
  penguins$flipper_length_mm[301] * -1

penguins$body_mass_g[1] <- 8000 ## too high value for body mass

penguins$body_mass_g[50] <- 5522 ## subtly too large body mass 

# convert island to character so we can add a typo
penguins$island <- as.character(penguins$island)
penguins$island[40] <- " Dream " ## add a space before and after island name

## convert species and island to a factor, now that we've added a typo
penguins$speciesL <- as.factor(penguins$speciesL)
penguins$island <- as.factor(penguins$island)

# finding errors

## lets try some reproducible approaches to identify these errors

# 1. "adelaie" typo -------------------------------------------------------

## we can calculate something, with some assertions upstream
## if the assertions fail, the calculation will also fail

## calculate the mean flipper_length_mm for each species
penguins %>% 
  group_by(species) %>% # group by species
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE)) # na.rm removes NA values

## note: if we had grouped by speciesL instead of species, we would
## already be able to notice the typo we introduced.
penguins %>% 
  group_by(speciesL) %>% # group by Latin species
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))

## here we can see that there is a group for adelaie (typo!)

## or you could count the number of observations in each Latin species 
## name to find the error
count(penguins$speciesL)

## or visualize the data!

ggplot(penguins, aes(factor(speciesL), body_mass_g)) +
  geom_boxplot(outlier.colour="black", outlier.size=0.5, 
               position = position_dodge(preserve = "single")) 

## this also highlights the outlier introduced to body mass

# 2. Deleted rows ---------------------------------------------------------

# a priori constraint: should include 344 observations

penguins %>% 
  verify(nrow(.) == 344) %>% 
  group_by(species) %>% 
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))

## stopped execution because there aren't 344 rows 
## change to 340 observations

penguins %>% 
  verify(nrow(.) == 340) %>% 
  group_by(species) %>% 
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))

# we can now see that 4 observations are missing

## you could also simply use the count() function to determine how many
## observations are in your data

count(penguins)

# 3. Negative flipper length ---------------------------------------------
# a priori constraint: all measurements should be positive

penguins %>% 
  assert(within_bounds(0, Inf), 
         -c(species, speciesL, island, sex, year)) %>%
  group_by(species) %>%
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE))

## stopped execution because flipper_length_mm includes a negative value.
## note that assert(), unlike verify(), identifies which element fails
## the test.

## you could also visualize your data and very quickly see that you have a negative
## value that falls outside what is to be expected

hist(penguins$flipper_length_mm)

# 4. Body mass outlier --------------------------------------------------
# post hoc constraint: body mass too heavy (outlier)

penguins %>% 
  insist(within_n_sds(2), body_mass_g) %>% 
  group_by(species) %>% 
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE))

## stopped execution because at least one value (8000) is more than 2 SD
## away from the global mean body_mass_g (i.e, across all species)
## it also identified 6 other potential outliers

## Tony Fischetti writes: "The problem with within_n_sds() is the mean 
## and standard deviation are so heavily influenced by outliers, their 
## very presence will compromise attempts to identify them using these 
## statistics. In contrast with within_n_sds(), within_n_mads() uses 
## the robust statistics, median and median absolute deviation, to 
## identify potentially erroneous data points."

penguins %>% 
  insist(within_n_mads(2), body_mass_g) %>% 
  group_by(species) %>% 
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE))

## note that insist() + within_n_mads() found many more potential 
## outliers for body_mass_g (13 total) than the one extreme value we 
## introduced. Also note that insist() found the way too large value
## (8000) but not the value that was subtly too large for given species
## (5522) because only 8000 is large relative to median and SD of the
## entire dataset

# visualize the data

hist(penguins$body_mass_g, nclass = 30)

## we would need to look at just the Adelie data to see the other 
## subtle outlier

hist(penguins$body_mass_g[penguins$species == "Adelie"], 
     nclass = 30)

## we can also see body_mass_g = 5522 is an outlier because it is too
## heavy given the flipper_length_mm of the individual

ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g, 
                     colour = species)) + 
  geom_point(size = 2, position = position_jitter(w = 0.2, h = 0.2)) + 
  facet_wrap(~ species, nrow = 2, scales = "free") + 
  theme(legend.position = "none")


# 5. Space before and after island name  ---------------------------------

## you can check for name errors using similar methods as previously describes:

# table will summarize the observation count of a factor

table(penguins$island)

# here you see there are two categories for Dream

## visualize the data

ggplot(penguins, aes(factor(island), body_mass_g)) +
  geom_boxplot(outlier.colour="black", outlier.size=0.5, 
               position = position_dodge(preserve = "single"))

# again, you can see there are two categories for Dream


## lets fix this:
# strip/trim the white space off of either end of the island name:

penguins <- penguins %>% 
    mutate(island = str_trim(island, side = "both"))

# double check
table(penguins$island)

# much better!