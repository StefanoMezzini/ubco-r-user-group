# see the cheat sheets by going to Help -> Cheat sheets -> ...

#' *vector operations* ----
x <- seq(from = 1, to = 10, by = 1)
x

#' add 1 to each value of `x` individually
x[1] <- x[1] + 1
x[2] <- x[2] + 1
x[3] <- x[3] + 1
x[4] <- x[4] + 1
x[5] <- x[5] + 1
x[6] <- x[6] + 1
x[7] <- x[7] + 1
x[8] <- x[8] + 1
x[9] <- x[9] + 1
x[10] <- x[10] + 1
x

#' add 1 to each value of `x` vectorially
x + 1

#' `for` *loops* ----
#' create a list of 5 vectors of 10 random normal values
y <- list()
y[[1]] <- rnorm(n = 10)
y[[2]] <- rnorm(n = 10)
y[[3]] <- rnorm(n = 10)
y[[4]] <- rnorm(n = 10)
y[[5]] <- rnorm(n = 10)
y

#' create the list using `for` loops
for(i in 1:5) {
  y[[i]] <- rnorm(n = 10)
}
y

#' create the list using `apply()`
y <- lapply(X = rep(x = 10, 5), FUN = rnorm)
y

#' summarize each vector of `y` to its mean
lapply(y, mean)

#' summarize as above, but simplify the list to a vector
#' (this only works if all lements are of the same type)
sapply(y, mean)

#' repeat this in a tibble
library('dplyr') # for data wrangling
tibble(samples = lapply(rep(x = 10, 5), rnorm),
       means = sapply(samples, mean))

#' repeat this using tidy functions
library('purrr') # for tidy functional programming
tibble(samples = map(.x = rep(x = 10, 5), .f = rnorm), # all allowed (list)
       means = map_dbl(samples, mean)) # only doubles allowed

#' more functions from `purrr`
d <-
  tibble(start = 1:10,
       int = map_int(start, function(number) number + 1L), # integers only
       let = map_chr(int, \(num) letters[num]),            # characters only
       dec = map_dbl(start, \(num) num + pi),              # numeric only
       even = map_lgl(int, \(num) num %% 2 == 0),            # TRUE/FALSE only
       descr = map2_chr(.x = int, .y = even,
                        .f = \(..x, ..y) if_else(..y,
                                                 paste(..x, 'is even.'),
                                                 paste(..x, 'is odd.'))))
d

# remember that vectorial operations are faster though!
mutate(d,
       descr_2 = if_else(even, paste(int, 'is even.'), paste(int, 'is odd.')))

#' common error with `map_***()` if vector type is wrong
map_dbl(1:10, as.character)

#' also see `map_df()`, `map_dfr()`, and `map_dfc()` for working with tibbles
