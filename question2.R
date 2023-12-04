game_simulation <- function(n){
  
  lowerbound <- 1/n
  upperbound <- 1 - (1/n)
  
  random_numbers <- runif(n, 0, 1)
  i <- 1
  while(i < n){
    if(random_numbers[i] < lowerbound || random_numbers[i] > upperbound ){
      return (-1)
    }
   i<- i + 1
  }
  return (10)
}

expected_payoff <- function(n){
  net_payoff <- 0
  c <- 10000
  while(c > 0){
    net_payoff <- net_payoff + game_simulation(n)
    c <- c -1
  }
  expected_value <- net_payoff / 10000
  return(expected_value)
}



count <- 1000
n_10_biggest <-0
n_100_biggest <-0
n_1000_biggest <-0
#set.seed(123)
while(count > 0){
  payoff_10 <- expected_payoff(10)
  payoff_100 <- expected_payoff(100)
  payoff_1000 <- expected_payoff(1000)
  
  if(payoff_10 > payoff_100 && payoff_10 > payoff_1000){
    n_10_biggest<- n_10_biggest + 1
  }
  
  if(payoff_100 > payoff_10 && payoff_100 > payoff_1000){
    n_100_biggest<- n_100_biggest + 1
  }
  
  if(payoff_1000 > payoff_100 && payoff_1000 > payoff_10){
    n_1000_biggest <- n_1000_biggest + 1
  }
  
  count <- count - 1
}

  


