#' flow_acc_md8_iteration
#'
#' @param input Character. Path to the digital elevation model file (with extension).
#' @param quantiles Numeric vector. Quantiles to compute, between 0 and 1. Use NULL to skip quantiles.
#' @param error Numeric. RMSE of the LiDAR elevation error, in meters.
#' @param n_iter Integer vector. Iteration numbers at which a result raster is written.
#' @param method Character. Flow accumulation algorithm: "md8" (whitebox::wbt_fd8_flow_accumulation)
#' or "d8" (whitebox::wbt_d8_flow_accumulation).
#' @param dispersion_statistics Logical. If TRUE, compute the standard deviation (sd) and
#' coefficient of variation (cv).
#' @param working_directory Character. Working directory where temporary files are written.
#'
#' @returns
#' a spatraster with multiple layers of flow accumulation distribution
#' @export
#'
#' @examples
#' # no example
flow_acc_md8_iteration <- function(input,
                                   quantiles = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
                                   error = 0.16,
                                   n_iter = c(2, 5, 10, 50, 100, 500),
                                   method = c("md8", "d8"),
                                   dispersion_statistics = TRUE,
                                   working_directory = getwd()){

  method <- match.arg(method)

  dem <- terra::rast(input)

  acc_temp <- list()

  for(i in 1:max(n_iter)){

    cat("Iteration", i, "\n")

    acc_temp[i] <- tempfile(tmpdir = working_directory, fileext = ".tif")
    dem_error_temp <- tempfile(tmpdir = working_directory, fileext = ".tif")
    dem_breached_temp <- tempfile(tmpdir = working_directory, fileext = ".tif")

    error_raster <- random_raster(raster = dem,
                               error = error,
                               seed = i)

    dem_error <- dem + error_raster

    terra::writeRaster(dem_error, dem_error_temp)

    breach_depression(input = dem_error_temp,
                      output = dem_breached_temp)

    if(method == "md8"){

      whitebox::wbt_fd8_flow_accumulation(dem = dem_breached_temp,
                                          output = acc_temp[i])

    } else {

      whitebox::wbt_d8_flow_accumulation(input = dem_breached_temp,
                                         output = acc_temp[i])

    }

    file.remove(dem_error_temp,
                dem_breached_temp)

    if(any(i == n_iter)){
      cat("Write final acc raster", i, "\n")

      # Read all acc temp
      acc_list <- lapply(acc_temp, terra::rast)
      acc <- terra::rast(acc_list)

      if(i == 1){

        final_acc <- terra::app(acc, "max")

      } else {

        # Compute min, max and mean
        cat("Compute min, max and mean\n")

        acc_min <- terra::app(acc, "min")
        acc_max <- terra::app(acc, "max")
        acc_mean <- terra::app(acc, "mean")

        final_acc <- c(acc_min,
                       acc_max,
                       acc_mean)

        # Compute sd and cv
        if(dispersion_statistics){

          cat("Compute sd and cv\n")

          acc_sd <- terra::app(acc, "sd")
          acc_cv <- (acc_sd / acc_mean)
          names(acc_cv) <- "cv"

          final_acc <- c(final_acc,
                         acc_sd,
                         acc_cv)

        }

        # Compute quantiles
        if(!is.null(quantiles)){

          acc_quantiles <- lapply(quantiles,
                                  function(x){

                                    cat("Compute quantile", x*100 ,"\n")

                                    terra::quantile(acc, x)

                                  })

          acc_quantiles <- terra::rast(acc_quantiles)

          final_acc <- c(final_acc,
                         acc_quantiles)

        }

      }

      terra::writeRaster(final_acc, paste0("./facc_iteration_", i, ".tif"))
    }

  }

  file.remove(unlist(acc_temp))

  return(final_acc)

}
