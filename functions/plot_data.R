data <- ag




data_perCondition <- function(data, condition){
  # Just in case the condition is specified by name
    if(condition=="stationary"){ condition <- 1 }else{
    if(condition=="abrupt"){     condition <- 2 }else{
    if(condition=="gradual"){    condition <- 3 }else{
    if(condition=="mixed"){      condition <- 4 }}}}
  # Isolate data collected for this condition
  datos <- data[,,condition,]
  
}