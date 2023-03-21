dataset <- read.csv("house_Strategy (1).csv")
dataset <- subset(dataset,)
#install.packages("genalg")
library(genalg)
limit <- 20000
iter <- 100
evalFunc <- function(x) {
  current_solution_predicted <- x %*% dataset$predicted
  current_solution_GROSS.AREA <- x %*% dataset$GROSS.AREA
  if (current_solution_GROSS.AREA > limit)
    return(0) else return(-current_solution_predicted)
}
monitor <- function(obj) {
  print(paste("GENERATION:", obj$iter))
  print(obj$population[which.min(obj$evaluations), ])
}
GAmodel <- rbga.bin(size = nrow(dataset), popSize = nrow(dataset)^2, iters = iter, mutationChance = 0.01, elitism = T, evalFunc = evalFunc, monitorFunc = monitor)
cat(summary(GAmodel))
solution <- GAmodel$population[which.min(GAmodel$evaluations),]
dataset[solution == 1, ]
write.csv(dataset[solution == 1, ],"house_Strategy (1)_GeneticAlgorithm.csv",row.names=FALSE)
-GAmodel$best[iter]
