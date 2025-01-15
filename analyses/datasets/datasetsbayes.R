## Started 4 January 2025 ##
## By Lizzie ##

## Datasets to use for homework ##
## Three options ... ##

# Option one is the Palmer Penguins data, which is in an R package:
require("palmerpenguins") # https://allisonhorst.github.io/palmerpenguins/

# The next two options are from previous papers. 
# I suggest you load the datasets, and look at the meta-data (separate) and pick one
if(length(grep("lizzie", getwd()))>0) { 
setwd("~/Documents/git/teaching/hotstats/hotstatsmisc/homeworkbayes2025/analyses")
}

fastswim <- read.delim("input/damselflies/fastswim.dta", sep="\t")
head(fastswim) # check out fastswim.md for metadata

carnchompers <- read.csv("input/carnivores/carnivoreteeth.csv")
carnbs <- read.csv("input/carnivores/carnivorebodymass.csv")
head(carnchompers)
head(carnbs)
# check out carnivoreteeth.md for metadata