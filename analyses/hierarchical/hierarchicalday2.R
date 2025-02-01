## Created 31 January 2025 ##
## From other code ... ##
## By Lizzie ##

# Consider the two datasets (below) and report on the model you recommend fitting. 
# Would you fit a hierarchical model? Yes or no and why?
# If you would fit a hierarchical model, what exact type and why? Give the notation. 
# Once you finish that, fit the model and pull out the most relevant metric for the aim stated. 

rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(rstanarm)
options(mc.cores = parallel::detectCores()) # use all my cores, go for it!

## Dataset 1
# Data on judges' scores for pairs figure skaters across two programs: short (program) and artistic (performance). 
# Aim: You want to award an Olympic medal in figure skating to the best pair in the short program. 

if(length(grep("lizzie", getwd()) > 0)) {
  setwd("~/Documents/git/teaching/hotstats/hotstatsmisc/bayesdatatsetsnmore/analyses/hierarchical/figureskating")
} else{
  setwd("/home/boomdittyboom")
}

dat <- read.csv("input/OlympicGames_1932.csv")
head(dat)

prog <- subset(dat, criterion == "Program")

# There's no single RIGHT answer to this, but a hierarchical model for pai and judge makes
# a lot of sense since from a conceptual standpoint (each is sampled from a population) and ...
# this will naturally pull in high and low scores from judges but it would be good and ...
# pairs, but the best pair will remain the best
# Here's the model ...
memodel <- stan_lmer(score ~ (1 | judge) + (1 |pair), data = prog)

save(memodel, file="output/memodel.Rdata")

library(shinystan)
# launch_shinystan(memodel)


## Dataset 2
# You have gathered meta-analytic data on the day of various phenological events across diverse species, collected in different studies. 
# Aim: You want to best estimate the change over time in phenology across all species.

## Set flags 
runmodels <- TRUE # set to TRUE if you want to run the models

# doy is day of year of the various phenological events
d <- read.csv("..//loughnan2024/input/phenologyData.csv")
covariates1 <- read.csv("..//loughnan2024/input/lifeHistoryData.csv")

dcov <- merge(d, covariates1, by.x=c("species", "phenophase"), 
  by.y=c("species.name", "phenophase"), all.x=TRUE)

# To be safe, adjust the year -- here relative to 1900
dcov$yearadj <- dcov$year-1900

# Should we add study ID?
dcovspstudy <- aggregate(dcov[c("doy")], dcov[c("species","studyid")], FUN=length)
aggregate(dcovspstudy["studyid"], dcovspstudy[c("species")], FUN=length)
# There is only ONE species represented across multiple studies 
# This means the model will try to use the different species within a study to pull apart...
# species and study; looking at what this does with domaine knowledge made us NOT do it ... 
# but the model will run: see below ... 

## RStanarm (arm) with species and study as crossed effects
if(runmodels){
  arm1spstud <- stan_lmer(doy~1 + (1|studyid) + (yearadj|species), data=dcov)
  save(arm1spstud, file="..//output/arm1spstud.Rdata")
}

if(!runmodels){
  load("..//output/arm1spstud.Rdata")
}

# I also would NOT nest species in study since my aim is to ...
# best estimate the change over time in phenology across all species
# Instead, I recommend fitting just species and being up front about the problem in the paper

## RStanarm (arm) with species grouping only
if(runmodels){
  arm1sp <- stan_lmer(doy~yearadj|species, data=dcov)
  save(arm1sp, file="..//output/arm1sp.Rdata")
}

if(!runmodels){
  load("..//output/arm1sp.Rdata")
}

## RStan mdoel with species grouping only
library(rstan)

# Formatting for R stan (way faster than rstanarm here)
dcovnoNA <- subset(dcov, is.na(doy)==FALSE)
N <- nrow(dcovnoNA)
y <- dcovnoNA$doy
Nspp <- length(unique(dcovnoNA$species)) 
species <- as.numeric(as.factor(dcovnoNA$species))
year <- dcovnoNA$yearadj

if(runmodels){
  fit <- stan("..//stan/twolevelhierslope.stan", data=c("N","y","Nspp","species","year"), 
    iter=4000, chains=4)
  save(fit, file="..//output/rstan1sp.Rdata")

}
if(!runmodels){
  load("..//output/rstan1sp.Rdata")
}

## And here I manipulate the posteriors as requested in manipulateposteriors.R 

# grab Stan output rstanarm
head(summary(arm1sp))
arm1sppost <- as.data.frame(arm1sp)
spslopes <- arm1sppost[, grep("b\\[yearadj species", colnames(arm1sppost))]

# estimate slopes for primary consumers vs. primary producers
table(dcov$trophic.level)
primaryprod <- subset(dcov, trophic.level=="primary producer")
ppspp <- unique(primaryprod$species)
primarycons <- subset(dcov, trophic.level=="primary consumer")
pcspp <- unique(primarycons$species)
# I don't recommend doing things by numbers like below, but this is quick for now
which(sort(unique(dcov$species)) %in% ppspp)
which(sort(unique(dcov$species)) %in% pcspp)
spslopespp <- spslopes[, which(sort(unique(dcov$species)) %in% ppspp)]
spslopespc <- spslopes[, which(sort(unique(dcov$species)) %in% pcspp)]

par(mfrow=c(1,2))
hist(rowMeans(spslopespp), main="", xlab="1ary producers")
hist(rowMeans(spslopespc), main="", xlab="1ary consumers")
quantile(rowMeans(spslopespp), c(0.1, 0.9))
quantile(rowMeans(spslopespc), c(0.1, 0.9))

# grep Stan output RStan
# Starter code, not finished for this output!
# watch out, below uses summary, but you should work from full posteriors 
rstan1sppost <- extract(fit) # posterior!
sumer <- summary(fit)$summary
muparams <- sumer[grep("mu", rownames(sumer)), c("mean", "2.5%", "25%", "50%", "75%", "97.5%")]
sigmaparams <- sumer[grep("sigma", rownames(sumer)), c("mean", "2.5%","25%", "50%", "75%", "97.5%")]

spslopes <- sumer[grep("b\\[", rownames(sumer)), "mean"]

