source("./functions/auxiliary/color_palettes.R")
source("./functions/auxiliary/transformations.R")

data <- ag
condition <- 1

data_perCondition <- function(data, condition, sub=NA){
  # Just in case the condition is specified by name
    if(condition=="stationary"){ condition <- 1 }else{
    if(condition=="abrupt"){     condition <- 2 }else{
    if(condition=="gradual"){    condition <- 3 }else{
    if(condition=="mixed"){      condition <- 4 }}}}
  # Isolate data pertaining this condition (and participant)
  if(is.na(sub)){  datos <- data[,,condition,]      }
  else{            datos <- data[,,condition,sub]   }
  # Get the color palette designed for this condition
  color <- color_all_conditions(0.3)[[condition]]
  
}