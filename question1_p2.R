library(ggplot2)
library(dplyr)
max_calculator <- function(dt){
  delta_t <- dt
  mean <- 0
  n <- 1/delta_t
  sd <- sqrt(delta_t)
  T_values <- c(0)
  count <- 10000
  while(count > 0){
    temperature_changes <- rnorm(n, mean, sd)
    temps <- cumsum(c(0,temperature_changes))
    max_time <- 0
    max_temp <- 0
    i <- 1
    while(i <= n){
      if(temps[i] > max_temp){
        max_temp <- temps[i]
        max_time <- delta_t * (i-1)
      }
      i <- i+1
    }
    T_values <- c(T_values, max_time)
    count <- count -1
  }
  T_values <- data.frame(tmax = T_values)
  
  T_values %>%
  ggplot( aes(x=tmax) ) +
   geom_density(fill="#426cf5", color="#e9ecef", alpha=0.9)
}

max_calculator(0.5)
max_calculator(0.1)
max_calculator(0.01)
max_calculator(0.001)
max_calculator(0.0001)
max_calculator(0.000001)
