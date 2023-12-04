library(ggplot2)
library(dplyr)
p_values_calculator <- function(dt){
  delta_t <- dt
  mean <- 0
  n <- 1/delta_t
  sd <- sqrt(delta_t)
  P_values <- c(0)
  count <- 10000
  while(count > 0){
    temperature_changes <- rnorm(n, mean, sd)
    temps <- cumsum(c(0,temperature_changes))
    positive <- 0
    i <- 1
    while(i <= n){
      if(temps[i] > 0){
        positive <- positive + 1
      }
      i <- i +1
    }
    P <- positive/length(temps)
    P_values <- c(P_values, P)
    count <- count -1
  }
  P_values <- data.frame(p = P_values)
  
  P_values %>%
    ggplot( aes(x=p)) +
    geom_density(fill="#f58a42", color="#e9ecef", alpha=0.9)
}

p_values_calculator(0.5)
p_values_calculator(0.1)
p_values_calculator(0.01)
p_values_calculator(0.001)
p_values_calculator(0.0001)
p_values_calculator(0.000001)

