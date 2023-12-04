x <- 1:100
counter <- 0
recorded_hits <- numeric(10000)

while (counter < 10000) {
  x <- sample(x)  # randomize the 1-100 vector
  index <- 1
  total <- 0  # Reset total at the beginning of each iteration
  
  while (index <= 100) {  
    if (x[index] == index) {
      total <- total + 1
    }
    index <- index + 1
  }
  
  recorded_hits[counter + 1] <- total
  counter <- counter + 1
}

expected <- mean(recorded_hits)
variance <- var(recorded_hits)

print(paste("Expected: ", expected))
print(paste("Variance: ", variance))
