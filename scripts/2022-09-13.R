# setting up a GitHub repo ----
#' download *GitHub Desktop*, and create an account if needed
#' clone R User Group repo

#' if needed, install the `tidyverse` pkg as a background job before proceeding
library('tidyverse')

# base elements in R ----
# 1D arrays: vectors
c(1, 4, 6.2)
c('a', 'b', 'c')
c(Sys.Date(), Sys.Date() + 1)
c(TRUE, FALSE, NA)
1:6

#' `c()` will coerce all elements to a single class
c(1, 'a', Sys.Date(), TRUE)

# vector operations
c(1, 5, 6) + 3
c(1, 5, 6) * c(0, 1, 2)
c(1, 5, 6) * c(1, 2) # read the warning!
c(1, 5, 6, 10) * c(1, 2)

# 2D arrays: matrices
matrix(data = 1:8, nrow = 2)
matrix(data = 1:8, ncol = 2)
matrix(data = 1:8, ncol = 2, byrow = TRUE)

# matrix operations
matrix(1:9, ncol = 3)
matrix(1:9, ncol = 3) + 3
matrix(1:9, ncol = 3) * 2 # NOT matrix multiplication
matrix(1:9, ncol = 3) %*% 6:8 # matrix multiplication
matrix(1:9, ncol = 3) %*% matrix(1:6, ncol = 2)

# grouping objects with different and types: lists
l <- list(letters = c('a', 'b', 'c'),
          numbers = 1:10,
          today = Sys.Date())
l
l$letters
l$today

# lists with a table-like structure: data frames
#' note: `data.frame()` recycles elements if the longest vector is a multiple
data.frame(numbers = 1:10,
           letters = c('a', 'b', 'c'),
           today = Sys.Date())

data.frame(numbers = 1:10,
           letters = c('a', 'b'),
           today = Sys.Date())

# importing data ----
#' open `example-data.csv` with a simple text editor (notepad or similar software)
#' set the working directory via:
#' - `setwd()`;
#' - the *menu* using:
#'   * Session > Set Working Directory > To Source File Location,
#'   * Session > Set Working Directory > To Files Pane Location;
#' - the *project* from the github repo you cloned
d <- read.csv(file = 'data/example-data.csv') # edit the file location if needed
class(d) # check the class of `d`
d # print the entire data frame
head(d) # print the first 6 rows
tail(d) # print the last 6 rows
summary(d) # print a summary of the data.frame

#' import data using the `readr` package
library('readr')
d <- read_csv(file = 'data/example-data.csv',
              col_types = 'Did') # Date, integer, double (numeric)
summary(d)
d

#' import `test-data.csv` and with the following column types (run `?read_csv` for help):
#'  - *date*: date
#'  - *initials*: character
#'  - *volume_ml*: number or double
#'  - *with_buffer*: logical
#'  - *passed*: factor
read_csv('data/test-data.csv', col_types = '?')
# what are the parsing issues? see if you can edit the csv to fix them
