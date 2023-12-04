win <- 0
loss <- 0

timesToRun <- 10000

j <- 0
while (j < timesToRun)
{
  n <- 100
  randNums <- runif(n = 1000, min = 1, max = 10000)
  
  i <- 1
  actualBiggest <- 0
  while (i <= n)
  {
    if (randNums[i] > actualBiggest)
    {
      actualBiggest <- randNums[i]
    }
    i <- i + 1
  }
  
  x<-10
  skips <-n/x
  i<-1;
  currentMax <-0
  ourGuess<-0
  
  while(skips != 0 && i <= n)
  {
    if(randNums[i]>currentMax){
      currentMax<-randNums[i]
      skips<-skips-1
    }
    i<-i+1
    
  }
  while(i<=n)
  {
    if(randNums[i] > currentMax)
    {
      ourGuess<-randNums[i]
      break
    }
    i<-i+1
  }
  
  if (ourGuess == actualBiggest&& ourGuess !=0)
  {
    win <- win + 1
  }
  else
  {
    loss <- loss + 1
  }
  j <- j +1
  
}

win
loss

winProb<-win/timesToRun
winProb
  
 