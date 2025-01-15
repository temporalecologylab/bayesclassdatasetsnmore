## Started 21 March 2022 ##
## By Lizzie ##
## Copied 15 Jan 2025 from ...
## ~/Documents/git/teaching/hotstats/hotstatsbayes/analyses/misc/mcmcsampling.R ##

# MCMC sampling, pulling from my various notes # 

# Some resources that I like:
# https://www.aptech.com/resources/tutorials/bayesian-fundamentals/metropolis-hastings-sampler/
# https://jellis18.github.io/post/2018-01-02-mcmc-part1/ A Practical Guide to MCMC  -- this is a useful and easy read (it's really on a Part 1, Part 2 is just this https://jellis18.github.io/post/2021-01-16-mcmc-part2/)
# But most of these focus on different things to evaluate, this one below actually evaluates the posterior:
# https://twiecki.io/blog/2015/11/10/mcmc-sampling/

# Below follow this one (done in R) 
# http://eriqande.github.io/sisg_mcmc_course/s03-01-intro-mcmc-in-R.nb.html (in R)
# and this: http://rinterested.github.io/statistics/MCMC.html

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# So, to build a sampler we need some space to search ...
# Let's build that first, and this example uses an exponential
x <- seq(from = 0, to = 10, length=20)
# And plot it ....
y <- exp(-x)
plot(x,y)
lines(x,y) 

# Okay, so we need to build our 'target' (what we're searching for in the MCMC)
# and, again, that's an exponential

target <- function(x){
  if (x < 0) {
    return(0)
  } else {
    return(exp(-x))
  }
}

# You can put whatever you want above ...

# And now! We build a Metropolis–Hastings (MCMC sampler)

x <- rep(0, 1000)
x[1] <- 3     # this is just a starting value, which the example I am following set arbitrarily to 3
for (i in 2:1000) {
  currentx <- x[i - 1]
  proposedx <- currentx + rnorm(1, mean = 0, sd = 1)
  # Below we compare the new proposal and our current spot
  A <- target(proposedx) / target(currentx) 
   # Below we take a random uniform, if the ratio is larger then we replace with with the proposal
  # Ch 9 Statistical Rethinking: Finally, a random number between zero and one is generated (runif(1)), and the king moves, if this random number is less than the ratio of the proposal island’s population to the current island’s population (proposal/current).
  if (runif(1) < A) {
    x[i] <- proposedx
  } else {
    x[i] <- currentx
  }
}


plot(x)
quartz()
hist(x)
