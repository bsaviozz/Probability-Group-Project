library(ggplot2)
game <- function(x, n, max){
  win <- 0
  timesToRun <- 10000
  numSkips <- n / x
  
  j <- 0
  while (j < timesToRun)
  {
    randNums <- sample(1:max, n, replace=F)
    actualBiggest <- max(randNums)
    
    i <- 1
    currBiggest <- 0
    while (i <= numSkips)
    {
      if (randNums[i] > currBiggest)
      {
        currBiggest <- randNums[i]
      }
      i <- i + 1
    }
    
    finalGuess <- currBiggest
    i <- numSkips +1
    while (i <= n)
    {
      if (randNums[i] > currBiggest)
      {
        finalGuess <- randNums[i]
        break
      }
      i <- i + 1
    }
    
    if (finalGuess == actualBiggest)
    {
      win <- win + 1
    }
    j <- j +1
  }
  p <- win / timesToRun
}

count <- seq(2,25, by=0.5)
probabilities <- c(0)
for(i in count){
  probabilities <- c(probabilities, game(i, 100, 1000))
}
count <- c(0, count)
probabilities <- data.frame(x = count, p = probabilities)
ggplot(probabilities, aes(x=x, y=p)) +
  geom_line( color="#69b3a2", linewidth=1, alpha=0.9, linetype="solid") + 
  geom_point() +
  scale_x_continuous(labels = as.character(count), breaks = count)


count <- seq(2,25, by=0.5)
probabilities <- c(0)
for(i in count){
  probabilities <- c(probabilities, game(i, 1000, 10000))
}
count <- c(0, count)
probabilities <- data.frame(x = count, p = probabilities)
ggplot(probabilities, aes(x=x, y=p)) +
  geom_line( color="#3632a8", linewidth=1, alpha=0.9, linetype="solid") 


count <- seq(2,25, by=0.5)
probabilities <- c(0)
for(i in count){
  probabilities <- c(probabilities, game(i, 10000, 100000))
}
count <- c(0, count)
probabilities <- data.frame(x = count, p = probabilities)
ggplot(probabilities, aes(x=x, y=p)) +
  geom_line( color="#2fd4b5", linewidth=1, alpha=0.9, linetype="solid") 