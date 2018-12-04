library(tidyverse)

load('RadyTastyPastry.rda')


profit <- function(q,x){
  if (x < q){
    pro <- -1.25*q + 5 * x + 0.5 *(q-x)
  } else {
    pro <- -1.25*q + 5 * q + 0.5*0
  }
  return(pro)
}




demand <- lm(demand ~ price, RadyTastyPastry)

RadyTastyPastry$price <- 5

De_w5 <- 928.31 + 5 * -78.61

Ex_de <- rnorm(1000, mean = De_w5, sd = 144.571)

profit_l <- c()

for (q in c(0.5:1078.5)){
  pro_list <- c()  
  for (i in Ex_de){
    pro_list <- pro_list %>% append(profit(q,i))
    }
  profit_l <- profit_l %>% append(mean(pro_list))
}

print(paste('when order ',c(0.5:1078.5)[which.max(profit_l)],' amount, we will get the maximum profit of ',max(profit_l)))

      
