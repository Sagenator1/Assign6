#' Growth of Plant over time
#'
#' Compute the growth of a plant over time.
#' @param sun number of sun hours per week
#' @param water number of watering events per week
#' @param pot size of pot (diameter in feet) the plant is growing in
#' @return height of plant (in mm)
#' @return number of leaves (n)
#'
#' @author Kristin Art, Sage Davis, Daija Odom

compute_plant_growth = function(sun, water, pot = 1){

  #start with some error checking
  sun = ifelse((sun < 0 ), return("Sun cannot be less than 0"), sun)
  water = ifelse((water < 0), return("Water cannot be less than 0"), water)

  #compute plant growth and new leaves
  growth = (2*sun + 0.5*water)*pot
  leaves = 0.1*growth

  return(list(growth, leaves))
}



