#### Libraries ####

library(doParallel)
library(doRNG)
library(foreach)

## How many cores do you have?
detectCores()

ncores <- 2
registerDoParallel(ncores)

#### In serial ####
ndraw <- 1000
set.seed(2342)

x <- list()
for(i in 1:ncores){
  x[[i]] <- rnorm(ndraw)
}

sapply(x,mean)

set.seed(2342)

x <- foreach(i = 1:ncores) %do% {
  rnorm(ndraw)
}

sapply(x,mean)

#### Foreach ####

set.seed(2342)

x <- foreach(i = 1:ncores) %dopar% {
  rnorm(ndraw)
}

sapply(x,mean)

set.seed(2342)

x <- foreach(i = 1:ncores) %dorng% { 
  rnorm(ndraw)
}

sapply(x,mean)

stopImplicitCluster()


