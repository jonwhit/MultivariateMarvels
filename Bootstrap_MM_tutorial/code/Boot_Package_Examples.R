##### Boot Package in R ###############

#Step 0: Package preparation
install.packages('boot',dep=TRUE)
library(boot)

#Step 1: Define a function that calculates the metric of interest
#in this example, we are calculating the ratio of means 
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)

#Step 2: take a look at the data
city <- city #boot package dataset
head(city)

#Step 3: Use bootstrap to calculate that metric N times 
#boot(data, function, replicates, stype = indices, frequencies or weights)
output.city <- boot(city, ratio, R = 999, stype = "w")
output.city
plot(output.city)

##################################################################

#Bootstrap of R-squared estimate

# Author DataFlair
library(boot)
# Creating Function to obtain R-Squared from the data
r_squared <- function(formula, data, indices) {
  val <- data[indices,] # selecting sample with boot 
  fit <- lm(formula, data=val)
  return(summary(fit)$r.square)
} 
# Performing 1500 replications with boot 
output.cars <- boot(data=mtcars, statistic=r_squared, 
               R=1500, formula=mpg~wt+disp)
# Plotting the output
output.cars   #t* = bootstrap replicates

plot(output.cars)
# Obtaining a confidence interval of 95%
boot.ci(output.cars, type="norm")


##############################################


