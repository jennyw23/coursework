library(tidyverse)
library(scales)
library(modelr)

theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)
setwd("//wsl.localhost/Ubuntu/home/v-wangjen/jenny_projects/coursework/week3")


library(rmarkdown)
library(reticulate)

# Convert the .ipynb file to .Rmd
nb_rmd = rmarkdown:::convert_ipynb("complexity_control.ipynb")


n = 50
effect_size = 0.099
alpha = 0.5


# install the package if not already installed
install.packages("pwr")

library(pwr)

# given values
n <- 25
effect_size <- 0.099
alpha <- 0.5

# calculate power
power_result <- pwr.p.test(h = 0.060, n = 25, sig.level = alpha, alternative = "greater")
print(power_result$power)

```

############### VALUES #################
pa <- 0.294
pb <- 0.250
alpha <- 0.05
n = 25

### R APPROACH

# use power.prop.test to compute the sample size you need
pow_r = power.prop.test(p1=pa, p2=pb, sig.level=alpha, n = n, alternative="one.sided")
print(pow_r)
### SIMULATION APPROACH

## Computing power by direct simulation
run_experiment <- function(pa, pb, n, alpha) {
  na <- sum(rbinom(n, 1, pa))
  nb <- sum(rbinom(n, 1, pb))
  test <- prop.test(x = c(na, nb), n = c(n, n), alternative="greater", conf.level = alpha)
  test$p.value < alpha
}

compute_power <- function(pa, pb, n, alpha, r = 1000) {
  mean(replicate(r, run_experiment(pa, pb, n, alpha)))
}

pow <- compute_power(pa, pb, 50, alpha)
print(pow)
