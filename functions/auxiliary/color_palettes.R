###########################################################
#  Arbitrary color palettes used throughout all plots     #
###########################################################

color_stationary <- function(alpha){
  # We use a distinctive color per subject
  # Arbitrarily selected to go around a color wheel :)
  return(c(rgb(235/255,33/255,33/255,alpha), rgb(255/255,33/255,77/255,alpha),
           rgb(255/255,33/255,107/255,alpha), rgb(255/255,33/255,137/255,alpha),
           rgb(255/255,33/255,167/255,alpha), rgb(255/255,33/255,211/255,alpha),
           rgb(255/255,33/255,249/255,alpha), rgb(189/255,33/255,255/255,alpha),
           rgb(122/255,33/255,255/255,alpha), rgb(77/255,33/255,255/255,alpha),
           rgb(33/255,33/255,255/255,alpha), rgb(33/255,155/255,255/255,alpha),
           rgb(33/255,174/255,255/255,alpha), rgb(33/255,234/255,255/255,alpha),
           rgb(33/255,255/255,211/255,alpha), rgb(33/255,255/255,122/255,alpha),
           rgb(33/255,255/255,40/255,alpha), rgb(115/255,255/255,33/255,alpha),
           rgb(182/255,255/255,33/255,alpha), rgb(234/255,255/255,33/255,alpha),
           rgb(255/255,226/255,33/255,alpha), rgb(255/255,152/255,33/255,alpha),
           rgb(255/255,92/255,33/255,alpha), rgb(255/255,33/255,33/255,alpha)))
}



color_abrupt <- function(alpha){
  # We use a distinctive color per subject
  return(c(rgb(235/255,33/255,33/255,alpha), 
           rgb(255/255,33/255,249/255,alpha), 
           rgb(77/255,33/255,255/255,alpha),
           rgb(33/255,174/255,255/255,alpha), 
           rgb(33/255,255/255,122/255,alpha),
           rgb(255/255,226/255,33/255,alpha),
           rgb(107/255,183/255,125/255,alpha),
           rgb(228/255,160/255,249/255,alpha)))
}


color_all_conditions <- function(alpha){
  return(list("stationary (1)" = color_stationary(alpha),
              "abrupt (2)"     = color_abrupt(alpha),
              "gradual (3)"    = color_stationary(alpha),
              "mixed (4)"      = color_stationary(alpha)))
}

#data_perCondition(ag,1)
#data_perCondition(ag,2)