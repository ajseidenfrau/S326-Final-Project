# Add more arguments to your function. Every parameter that you need to compute the total deviation and 
# the constraints should be passed into the function as its arguments.
deviation <- function(Q, ...){
 
 # For every class, find the indices of the products in that class.
  f <-
  a <-
  d <-
  r <-
  b <-   

  # compute the absolute value for the deviation of the capacity assigned to each class from its target.    
  f_deviation <- 
  a_deviation <- 
  d_deviation <- 
  r_deviation <- 
  b_deviation <- 
  
  total_deviation <-f_deviation + a_deviation + d_deviation + r_deviation + b_deviation
  
  # return a penalty for infeasible solutions and the total_deviation for feasible solutions.
  if(  |   |    |    |    ){

    
  }else{
    
  }
}




#feasible  solution: Use Q_f as a feasible solution. The deviation function should return 288.512 as the output
# for this feasible solution.
best_home <- read.csv(file = "BestHomePart1.csv", 
                      header = T, as.is = T)

Q_f <- c()

for(i in 1:nrow(besthome)){
  Q_f[i] <- 0
}

# find indices of products in each class
f_in <- 
a_in <- 
b_in <- 
d_in <- 
r_in <-  


Q_f[f_in[1:10]] <- 12500/(10*mean(best_home$Required_capacity[f_in]))
Q_f[a_in[1:10]] <- 6000/(10*mean(best_home$Required_capacity[a_in]))
Q_f[b_in[1:10]] <- 3300/(10*mean(best_home$Required_capacity[b_in]))
Q_f[d_in[1:10]] <- 5800/(10*mean(best_home$Required_capacity[d_in]))
Q_f[r_in[1:10]] <- 2900/(10*mean(best_home$Required_capacity[r_in]))