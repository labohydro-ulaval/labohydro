#' Topographic position index from Weiss poster : https://www.jennessent.com/downloads/TPI-poster-TNC_18x22.pdf
#'
#' @param dem A `SpatRaster`. Digital elevation model.
#' @param inner_radius Integer. Inner radius of the annulus, in cells.
#' @param outer_radius Integer. Outer radius of the annulus, in cells.
#' @param round Logical. If `TRUE`, the index is rounded to the nearest integer.
#'
#' @returns
#' Topographic position index from Weiss poster
#' @export
#'
#' @examples
#' # no example
weiss_topographic_position_index <- function(dem,
                                             inner_radius,
                                             outer_radius,
                                             round = TRUE){

  # Apply focal mean with the donut-shaped window
  focal_mean <- terra::focal(dem,
                             w = annulus_focal_window(inner_radius, outer_radius),
                             fun = "mean",
                             na.policy = 'omit')

  # Calculate TPI: TPI = DEM - focal_mean + 0.5
  tpi <- dem - focal_mean + 0.5

  # Apply rounding to integer
  if(round == TRUE){
    tpi <- round(tpi)
  }

  return(tpi)
}
