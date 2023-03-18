#' Fish Diversity
#'
#' This function takes a vector of fish names and returns the most common species, the most rare species, and the total number of species
#' @param species vector of species
#' @return list with the following items
#' \itemize{
#' \item {name of the most common species}
#' \item {name of the rarest species}
#' \item {total number of species}
#' }
#' @author Daija Odom, Sage Davis, Kristin Art

# Fish data is the vector
compute_fish = function(fish_vector) {

  # Convert vector of species to a factor
  fish_factors = as.factor(fish_vector)

  # Calculate the most common species and just get the name
  most_common_fish = names(which.max(summary(fish_factors)))

  # Calculate the rarest species
  rarest_fish = names(which.min(summary(fish_factors)))

  # Calculate the total number of species
  total_fish = length(summary(fish_factors))

  # Use lists to return multiple, diverse pieces of information from function
  return(list(most_common_fish, rarest_fish, total_fish))
}

