#' circle_focal_window
#'
#' @param radius Integer. Radius of the circle, in cells. Must be 1 or more.
#'
#' @returns
#' a matrix of circle focal window, with 1 inside the circle and NA outside
#' @export
#'
#' @examples
#' # no example
circle_focal_window <- function(radius){

  if(radius < 1 || radius != round(radius)){
    stop("`radius` must be a whole number of cells, 1 or more.")
  }

  # Define the radius of the focal window (it must cover the outer radius)
  window_radius <- radius * 2 + 1  # total window radius is twice the outer radius + 1
  diameter <- radius + 1

  # Create an empty matrix for the focal window
  focal_window <- matrix(NA_real_, nrow = window_radius, ncol = window_radius) # terra would count 0 as a value

  # Fill the focal window to create a donut-shaped matrix
  # Mark cells within the outer radius but excluding the inner radius
  for (i in 1:window_radius) {
    for (j in 1:window_radius) {
      distance <- sqrt((i - diameter)^2 + (j - diameter)^2) # Compute the distance from the center cell
      if (distance <= radius) {
        focal_window[i, j] <- 1  # Set to 1 where the distance is within the radius
      }
    }
  }
  return(focal_window)
}
