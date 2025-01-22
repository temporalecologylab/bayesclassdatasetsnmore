## Started 25 January 2022 ##
## Let's simulate some data and do prior predictive checks ##

## Lizzie copied on 20 January 2025 from ...
## hotstatsmisc/hotstats_before2025/examples/exampleday3/exampleday3_priorpc.R

# This just shows a FEW prior predictive checks #
# You'd likely to do many more (and of various designs) #
# Consider this a very small sampler..... 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Let's simulate day of year of leafout as a f(x) of temperature

# Quick review of notation ... all the below are equivalent
# y <- a + bx + error
# y ~ normal(mu, error), where mu = a + bx
# ypred = a + bx; y ~ normal(ypred, error)

##
## One sample (test) dataset

# Start by defining the parameters
a <- 100  # intercept
b <- -0.5 # slope
sigmay <- 10 # error

# now we need x data
n <- 100
temperature <- rnorm(n, 10, 3)

# And we have what we need for creating y data 
y <- a + b*temperature + rnorm(n, 0, sigmay) # a + bx + error

# Let's plot this ...
plot(y~temperature, ylim=c(0, 365), xlim=c(0, 40), # set a range of possible values to see our data
     xlab="temperature (C)", ylab="day of leafout")
abline(a=a, b=b)

# Hmm, I would expect the effect per degree to be higher, so my slope is wrong
# Also, here I realize that at 0 C the plant may NEVER leafout ...
# So, if I want to use linear regression I might reframe it within a range of temperature in which that linear relationship is a good approximation ...
b <- -2
temperature <- rnorm(n, 15, 3)

# Take 2 ...
y <- a + b*temperature + rnorm(n, 0, sigmay) # a + bx + error

plot(y~temperature, ylim=c(0, 365), xlim=c(0, 40), 
     xlab="temperature (C)", ylab="day of leafout")
abline(a=a, b=b)


##
## One prior predictive check (prior PC) ... we eventually want to do this for all parameters
## For any parameter, the prior needs a distribution and values within that distribution ...

# Let's do b, we think it's normally distributed so we need to define mean (mu) and variation (sigma)
mub <- -2 # mean (mu), I start with what seemed okay above
sigmab <- 0.5 # variation

# Step 1 -- always plot the basic histogram first!
hist(rnorm(10000, mub, sigmab))

# Seems okay, on to Step 2...

ppcreps <- 200 # up to you, more is better generally
bprior <- rnorm(ppcreps, mub, sigmab)

plot(1, type="n", ylim=c(0, 365), xlim=c(5, 40), # zoom in on relevant temperatures
     xlab="temperature (C)", ylab="day of leafout")
for (i in c(1:ppcreps)){
    abline(a=a, b=bprior[i])
    }

# At this point, I would likely increase the variation of b's prior
# And add in some intercept variation ...
mua <- 100 
sigmaa <- 30 
hist(rnorm(10000, mua, sigmaa))

aprior <- rnorm(ppcreps, mua, sigmaa)

plot(1, type="n", ylim=c(-50, 200), xlim=c(5, 40),
     xlab="temperature (C)", ylab="day of leafout")
for (i in c(1:ppcreps)){
    abline(a=aprior[i], b=bprior[i])
    }

# Does this cover ALL vaguely possible outcomes?
# If not, you need to change your priors ...
# If yes, do they have any INSANE outcomes? (Again, change your priors)

# Another plotting option (you need to design prior PC) ...
sigmaymu <- 10
sigmaysigma <- 1

sigmayprior <- rnorm(ppcreps, sigmaymu, sigmaysigma)

par(mfrow=c(5, 5))
for (i in c(1:25)){
    ydatahere <- a + b*temperature + rnorm(100, 0, sigmayprior[i])
    plot(ydatahere~temperature,
         ylim=c(-50, 200), xlim=c(5, 40),
         xlab="temperature (C)", ylab="day of leafout",
         main=paste("sigma=", round(sigmayprior[i], 2)))
    abline(a=a, b=b)
}

# Hmm, that seems small, I would likely change that prior based on this check
