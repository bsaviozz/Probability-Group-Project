rolling_dice <- function(){
  dice1 <- sample(1:6, 1)
  dice2 <- sample(1:6, 1)
  if(dice1 == 6 && dice2 == 6){
    return (TRUE)
  }
  else{
    return (FALSE)
  } 
}

game <- function(n, max){
  #create vector with n random numbers
  papers <- sample(1:max,n, replace=F) 
  
  #pick paper if double six
  my_paper <- 0
  for(i in papers){
    if(rolling_dice()){
      my_paper <- i
      break
    }
  }
  actual_max <- max(papers)
  if(actual_max == my_paper){
    return (TRUE)
  }
  else{
    return (FALSE)
  }
}

monte_carlo <- function(n, max){
  c <- 10000
  wins <- 0
  while (c > 0) {
    if(game(n, max)){
      wins <- wins + 1
    }
    c <- c - 1
  }
  prob_of_wining <- wins / 10000
}

p <- monte_carlo(100, 1000)
print(p)
