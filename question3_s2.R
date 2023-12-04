library(ggplot2)
game <- function(x, n, max){
  win <- 0
  timesToRun <- 10000
  j <- 0
  while (j < timesToRun){
    randNums <- sample(1:max, n, replace=F)
    actualBiggest <- max(randNums)
    skips <- x
    
    i<-1
    currentMax <-0
    while(skips != 0 && i <= n){
      if(randNums[i]>currentMax){
        currentMax<-randNums[i]
        skips <- skips-1
      }
      i<-i+1
    }
    
    ourGuess<- currentMax
    while(i<=n){
      if(randNums[i] > currentMax){
        ourGuess<-randNums[i]
        break
      }
      i<-i+1
    }
  
    if (ourGuess == actualBiggest && ourGuess !=0)
    {
      win <- win + 1
    }
    j <- j +1
  }
  p <- win / timesToRun
  return(p)
}

count <- seq(2,25)
probabilities <- c(0)
for(i in count){
  probabilities <- c(probabilities, game(i, 100, 1000))
}
count <- c(0, count)
probabilities <- data.frame(x = count, p = probabilities)
ggplot(probabilities, aes(x=x, y=p)) +
  geom_line( color="#69b3a2", linewidth=1, alpha=0.9, linetype="solid") 

count <- seq(2,25)
probabilities <- c(0)
for(i in count){
  probabilities <- c(probabilities, game(i, 1000, 10000))
}
count <- c(0, count)
probabilities <- data.frame(x = count, p = probabilities)
ggplot(probabilities, aes(x=x, y=p)) +
  geom_line( color="#3632a8", linewidth=1, alpha=0.9, linetype="solid") 

count <- seq(2,25)
probabilities <- c(0)
for(i in count){
  probabilities <- c(probabilities, game(i, 10000, 100000))
}
count <- c(0, count)
probabilities <- data.frame(x = count, p = probabilities)
ggplot(probabilities, aes(x=x, y=p)) +
  geom_line( color="#2fd4b5", linewidth=1, alpha=0.9, linetype="solid") 

 
