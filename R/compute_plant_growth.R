#' Growth of Plant over time
#'
#' Compute the growth of a plant over time based on environmental conditions.
#' @param conditions dataframe containing 'sun' column with number of sun hours per week (n) and 'water' column containing number of watering events per week (n)
#' @param pot size of pot (diameter in feet) the plant is growing in
#' @return list with the following items
#' \itemize{
#' \item {height of plant (mm)}
#' \item {number of leaves (n)}
#' }
#' @author Kristin Art, Sage Davis, Daija Odom

compute_plant_growth = function(conditions, pot = 1){
  sun <- conditions$sun
  water <- conditions$water
  #start with some error checking
  sun = ifelse((sun < 0 ), return("Sun cannot be less than 0"), sun)
  water = ifelse((water < 0), return("Water cannot be less than 0"), water)

  #compute plant growth and new leaves
  growth = (2*sun + 0.5*water)*pot
  leaves = 0.1*growth

  return(list(growth, leaves))
}



