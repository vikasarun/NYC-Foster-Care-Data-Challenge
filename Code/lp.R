install.packages('slam') 
library('slam')
install.packages('/Library/gurobi701/mac64/R/gurobi_7.0-1.tgz', repos=NULL)
library('gurobi')

#https://www.gurobi.com/documentation/7.0/quickstart_mac/r_example.html


constraint_vec = c()
for (j in 1:8){
  cmatrix = rep(0,80)
  for (i in 0:9){
    cmatrix[j + 8*i] = 1
  }
  constraint_vec = c(constraint_vec,cmatrix)
}



for (j in 0:9){
  pmatrix = rep(0,80)
  for (i in 1:10){
    pmatrix[i + 8*j] = 1
  }
  constraint_vec = c(constraint_vec,pmatrix)
}

#constraint_vec = seq(1:1440)
constraint_matrix = matrix(constraint_vec, nrow=18, ncol=80,byrow=T)

model <- list()


model$A <- constraint_matrix



#turns probability matrix into vector, goes down values in column 1, then down values in column 2
model$obj <- c(LP_matrix)
model$modelsense <- "max"

#find right hands side (18 elements)
model$rhs <- c(rowSums(size_matrix),colSums(size_matrix))


model$sense <- rep('=',18)

model$vtype <- 'B'

params <- list(OutputFlag=0)

result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)
