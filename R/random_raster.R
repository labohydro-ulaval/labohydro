#' random_raster
#'
#' @param raster A `SpatRaster`. Reference raster.
#' @param error Numeric. RMSE of the random error.
#' @param seed Integer. Random seed for reproducibility.
#' @param radius Integer. Radius, in cells, of the circular mean focal statistic that
#'   smooths the random field, from `circle_focal_window()`. Larger windows produce a
#'   smoother (more spatially autocorrelated) error field. Default `1`.
#'
#' @returns
#' a random raster
#' @export
#'
#' @examples
#' # no example
random_raster <- function(raster,
                          error,
                          seed = 1,
                          radius = 1){

  # Generate a new raster with the same extent and resolution as the input DEM
  raster_random <- terra::rast(raster)  # Copy properties from DEM raster
  # Generate random values from a normal distribution using the specified mean and standard deviation
  set.seed(seed) # Set a random seed for reproducibility
  terra::values(raster_random) <- stats::rnorm(terra::ncell(raster_random), mean = 0, sd = 1)

  terra::mask(raster_random, raster) -> raster_random_mask

  terra::focal(raster_random_mask, w = circle_focal_window(radius), fun = "mean") -> raster_random_foc

  raster_random_foc/terra::global(raster_random_foc, "sd", na.rm = T)$sd -> raster_random_foc_corr

  raster_random_foc_corr*error -> raster_random_foc_corr_err

  return(raster_random_foc_corr_err)

}
