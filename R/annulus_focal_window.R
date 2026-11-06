#' annulus_focal_window
#'
#' @param inner_radius Integer. Inner radius of the annulus, in cells. Must be 0 or more.
#' @param outer_radius Integer. Outer radius of the annulus, in cells. Must be 1 or more,
#'   and at least `inner_radius`.
#'
#' @returns
#' a matrix of annulus focal window, with 1 inside the annulus and NA outside
#' @export
#'
#' @examples
#' # no example
annulus_focal_window <- function(inner_radius,
                                 outer_radius){

  if(inner_radius < 0 || inner_radius != round(inner_radius) ||
     outer_radius < 1 || outer_radius != round(outer_radius) ||
     inner_radius > outer_radius){
    stop("`inner_radius` and `outer_radius` must be whole numbers of cells, with 0 <= inner_radius <= outer_radius and outer_radius >= 1.")
  }

  # Define the radius of the focal window (it must cover the outer radius)
  window_radius <- outer_radius * 2 + 1  # total window radius is twice the outer radius + 1
  diameter <- outer_radius + 1

  # Create an empty matrix for the focal window
  focal_window <- matrix(NA_real_, nrow = window_radius, ncol = window_radius) # terra would count 0 as a value

  # Fill the focal window to create a donut-shaped matrix
  # Mark cells within the outer radius but excluding the inner radius
  for (i in 1:window_radius) {
    for (j in 1:window_radius) {
      distance <- sqrt((i - diameter)^2 + (j - diameter)^2) # Compute the distance from the center cell
      if (distance >= inner_radius && distance <= outer_radius) {
        focal_window[i, j] <- 1  # Set to 1 where the distance is within the annulus
      }
    }
  }
  return(focal_window)
}
