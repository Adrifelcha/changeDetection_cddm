###########################################################
#  Arbitrary color palettes used throughout all plots     #
###########################################################

color_stationary <- function(alpha){
  # We use a distinctive color per subject
  # Arbitrarily selected to go around a color wheel :)
  return(c(rgb(235/255,223/255,53/255,alpha), rgb(235/255,144/255,53/255,alpha),
           rgb(235/255,53/255,53/255,alpha), rgb(235/255,53/255,174/255,alpha),
           rgb(204/255,53/255,235/255,alpha), rgb(126/255,53/255,235/255,alpha),
           rgb(78/255,53/255,235/255,alpha), rgb(53/255,72/255,235/255,alpha),
           rgb(53/255,132/255,235/255,alpha), rgb(53/255,211/255,235/255,alpha),
           rgb(53/255,235/255,120/255,alpha), rgb(84/255,235/255,53/255,alpha),
           rgb(204/255,235/255,53/255,alpha), rgb(235/255,198/255,53/255,alpha),
           rgb(235/255,96/255,53/255,alpha), rgb(235/255,53/255,53/255,alpha),
           rgb(235/255,53/255,108/255,alpha), rgb(235/255,53/255,235/255,alpha),
           rgb(114/255,53/255,235/255,alpha), rgb(53/255,96/255,235/255,alpha),
           rgb(53/255,229/255,235/255,alpha), rgb(53/255,235/255,96/255,alpha),
           rgb(217/255,235/255,53/255,alpha), rgb(235/255,96/255,53/255,alpha)))
}