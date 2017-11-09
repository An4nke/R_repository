library(doParallel)
library(microbenchmark)
library(party)

showConnections()

# show cores of computer
cpus <- detectCores()

cl = makeCluster(0.75 * cpus)
registerDoParallel(cl)

showConnections()

# foreach loop
foreach(i = 1:4) %do% sqrt(i) # %do% instead {}

foreach(i = 1:4, .combine = "c") %do% sqrt(i)

# cut
cut_lst <- split(diamonds, f = diamonds$cut)


models = foreach(i = cut_lst) %do% {
  lm(carat ~ price, data = i) # calculate lineare model foreach interation
}

# similar version
models = foreach(i = 1:length(cut_lst)) %do% {
  lm(carat ~ price, data = cut_list[[i]]) # calculate lineare model foreach interation
}


microbenchmark(
  { foreach(i = ls_diamonds) %do% lm(carat ~ price, data = i) },
  { foreach(i = ls_diamonds) %dopar% lm(carat ~ price, data = i)
}, times = 20L)

# remember closing connections
stopCluster(cl) # closeAllConnections()
showConnections()