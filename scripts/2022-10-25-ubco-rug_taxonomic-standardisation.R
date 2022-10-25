# Liam GW Johnson 2022-10-19

# taxonomic standardisation


# consider that we have a list of species names:

spec_df <- read.csv( file.path( "data", "eg-species-names_raw.csv") )

# (these are the tip names from Qian and Jin 2016 megaphylogeny of plants)


# for convenience, store them as a vector

spec <- as.vector(spec_df$species)


# almost certainly, this list is not "clean"; it likely contains outdated names,
# misspellings, "sp." (species epithet unknown), and other errors/issues

# accordingly, we need to carefully check our list of names, identify the oddities,
# and correct things where appropriate


# base R options:

head(spec)

tail(spec)

length(spec)

length( unique(spec) )

sum( duplicated(spec) )

spec[ which( duplicated(spec) ) ]

# if there are only a few species
#table(spec)


# look for a few telltale patterns

# typical plant families end in "aceae"
grep("aceae", spec)

spec[ grep("aceae", spec) ]

spec[ grep("sp\\.", spec) ]

spec[ grep("sp$", spec) ]

spec[ grep("unk", spec) ]

grep(" $", spec) # for trailing spaces



# sometimes we compare our list to a second list

#identical(spec, spec2)

#length( intersect(spec, spec2) )

#setdiff(spec, spec2); setdiff(spec2, spec)



# when dealing with codes etc. that should follow a specific format

table( nchar(spec) )


# making corrections

spec <- gsub("_", " ", spec) # change underscores to spaces

spec[ which(spec == "Blasia pusilla") ] <- "Blasia pusillaa"

spec <- ifelse(spec == "Blasia pusillaa", "Blasia pusilla", spec)


spec <- gsub("sp$", "sp\\.", spec)


# once we have a list of what looks like good species names, we can compare them to a
# database of recognized names

# for this example we'll use the taxise package, but depending on what group of
# organisms you work with, other methods may be more applicable

library(taxize)

# basic tutorial: https://docs.ropensci.org/taxize/articles/taxize.html


spec_subset <- spec[1:3] # get a small subset to work with - these functions can be slow


# Global Names Resolver

gnr_out <- gnr_resolve(spec_subset)

head(gnr_out, n = 10)

setdiff(gnr_out$matched_name, gnr_out$user_supplied_name)


# get higher level taxonomy

classification(spec_subset, db = "itis")



# for plants / The Plant List:

library(Taxonstand)

TPL(spec_subset)


