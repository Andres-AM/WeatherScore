

source("libraries.R")


x <- NA

nbYear <- function (p0, percent, aug, p) {
  
  x[1] <- p0
  n <- 4

  for(i in c(2:n)){ x[i] <- round(x[i-1] + x[i-1]*(percent/100)+aug) }

  return(x[n])
  
}

nbYear(100,5,10, 100)

nbYear <- function(p0, percent, aug, p) {
  x <- numeric(1)  # Initialize an empty vector to store population values
  x[1] <- p0        # Initial population
  n <- 1            # Initialize years counter
  
  # Iterate until the population reaches or exceeds target population p
  while (x[n] < p) {
    n <- n + 1
    x[n] <- round(x[n - 1] * (1 + percent / 100) + aug)
  }
  
  return(n - 1)  # Return the number of years needed
}


