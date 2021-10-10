
# creating some example variables
X <- rnorm(n=50, mean = 10, sd = 5)
Y <- rnorm(n=50, mean = 15, sd = 6)
Z <- rnorm(n=50, mean = 20, sd = 5)
K <- rnorm(n=50, mean = 30, sd = 5)
Data <- data.frame(X, Y, Z, K)

# In the data set all the variables should be numeric

#install.packages("plyr")
library(plyr)
combos <- combn(ncol(Data),2) # all combinations taking 2 at a time

result <- adply(combos, 2, function(x) {
  # t test defining 
  test <- t.test(Data[, x[1]], Data[, x[2]], 
                 alternative = "two.sided", var.equal = TRUE)
  
  # making data frame
  out <- data.frame("Var 1" = colnames(Data)[x[1]],
                    "Var 2" = colnames(Data[x[2]]),
                    "t value" = sprintf("%.3f", test$statistic),
                    "df"= test$parameter,
                    "p value" = sprintf("%.4f", test$p.value),
                    "CI" = paste(format(test$conf.int, digits = 3), 
                                       collapse = ",")
  )})[,-1]

View(result)

# Thanks to this question in stackoverflow
# https://stackoverflow.com/questions/9661469/r-t-test-over-all-columns
