# A custom function to determine the number of columns and rows necessary
# to display any number of plots into a grid
grid_size <- function(number){
  if(number<=3){  return(c(1,number))
  }else{
        x <- sqrt(number)
        if(x %% 1 == 0){  return(c(x,x))
        }else{
              rows <- ceiling(x)
              columns <- ceiling(x)
              if(rows*columns>=number){ return(c(rows,columns))
              }else{  return(c(rows+1,columns))}
            }
      }
}