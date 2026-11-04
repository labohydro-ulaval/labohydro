#' watershed
#'
#' @param facc A `SpatRaster`. D8 flow accumulation.
#' @param fdir A `SpatRaster`. D8 flow direction.
#' @param pour_point An `sf` object. Pour points.
#' @param output_directory Character. Working directory where temporary files are written.
#' @param snap_distance Numeric. Maximum distance to snap pour points to the flow accumulation, in map units.
#' @param remove_temp Logical. If `TRUE`, temporary files (facc_temp, fdir_temp, pour_point_temp, snap_pour_point_temp, watershed_raster_temp, watershed_polygon_temp) are deleted.
#' @param esri_pntr Logical. If `TRUE` (default), `fdir` uses the ESRI D8 pointer scheme (as produced by `flow_dir_acc()`).
#'
#' @returns
#' same watershed number as pour point
#' @export
#'
#' @examples
#' # no example
watershed <- function(facc,
                      fdir,
                      pour_point,
                      output_directory = getwd(),
                      snap_distance = 10,
                      remove_temp = TRUE,
                      esri_pntr = TRUE) {

  # ------------------------------------------------------------
  # 1. Prepare working directory
  # ------------------------------------------------------------
  old_wd <- getwd()
  setwd(output_directory)

  # ------------------------------------------------------------
  # 2. Create named temporary files in working directory
  # ------------------------------------------------------------
  facc_temp                <- "facc_temp.tif"
  fdir_temp                <- "fdir_temp.tif"
  pour_point_temp          <- "pour_point_temp.shp"
  snap_pour_point_temp     <- "snap_pour_point_temp.shp"
  watershed_raster_temp    <- "watershed_raster_temp.tif"
  watershed_polygon_temp   <- "watershed_polygon_temp.shp"

  # ------------------------------------------------------------
  # 3. Write input rasters and pourpoints to disk
  # ------------------------------------------------------------
  cat("Writing base data...\n")
  terra::writeRaster(facc, facc_temp, overwrite = TRUE)
  terra::writeRaster(fdir, fdir_temp, overwrite = TRUE)
  sf::st_write(pour_point, pour_point_temp, quiet = TRUE, delete_dsn = TRUE)

  # ------------------------------------------------------------
  # 4. Watershed delineation through WhiteboxTools
  # ------------------------------------------------------------
  cat("Creating watershed...\n")

  whitebox::wbt_snap_pour_points(
    pour_pts  = pour_point_temp,
    flow_accum = facc_temp,
    output     = snap_pour_point_temp,
    snap_dist  = snap_distance
  )

  whitebox::wbt_watershed(
    d8_pntr = fdir_temp,
    pour_pts = snap_pour_point_temp,
    output = watershed_raster_temp,
    esri_pntr = esri_pntr
  )

  whitebox::wbt_raster_to_vector_polygons(
    input  = watershed_raster_temp,
    output = watershed_polygon_temp
  )

  # ------------------------------------------------------------
  # 5. Clean watershed polygons
  # ------------------------------------------------------------
  cat("Cleaning watershed polygons...\n")

  ws <- sf::st_read(watershed_polygon_temp, quiet = TRUE)
  ws <- sf::st_make_valid(ws)
  ws <- sf::st_transform(ws, 6622)

  # Keep ID column (VALUE)
  watershed_combined <- ws["VALUE"]
  names(watershed_combined) <- "id"

  # ------------------------------------------------------------
  # 6. Clean pourpoints & assign IDs matching watershed polygons
  # ------------------------------------------------------------
  cat("Cleaning pour points...\n")

  pp <- sf::st_read(snap_pour_point_temp, quiet = TRUE)
  pp <- sf::st_make_valid(pp)
  pp <- sf::st_transform(pp, 6622)

  # Create ID = FID + 1 for consistency with watershed ID
  pp$id <- pp$FID + 1
  pour_point_combined <- pp["id"]

  # ------------------------------------------------------------
  # 7. assign nearest downstream watershed for each polygon
  # ------------------------------------------------------------
  cat("Assigning downstream watershed IDs...\n")

  n_ws <- nrow(watershed_combined)
  to_watershed <- vector("list", n_ws)

  for (i in seq_len(n_ws)) {
    this_id <- watershed_combined$id[i]

    # Extract pour point matching ID
    pp_i <- pour_point_combined[pour_point_combined$id == this_id, ]

    # Candidate watersheds except itself
    ws_others <- watershed_combined[watershed_combined$id != this_id, ]

    # Find nearest watershed polygon
    nearest_idx <- sf::st_nearest_feature(pp_i, ws_others)
    nearest_poly <- ws_others[nearest_idx, ]

    # Compute geometric distance
    dist_m <- as.numeric(sf::st_distance(pp_i, nearest_poly))

    # Only assign if close enough
    if (dist_m < 10) {
      to_watershed[[i]] <- nearest_poly$id
    } else {
      to_watershed[[i]] <- NA
    }
  }

  watershed_combined$to_watershed <- unlist(to_watershed)

  # ------------------------------------------------------------
  # 8. Combine upstream watersheds
  # ------------------------------------------------------------
  cat("Combining upstream watersheds...\n")

  all_results <- list()

  for (i in seq_len(n_ws)) {
    target_id <- watershed_combined$id[i]

    # Start with the main watershed
    selected <- watershed_combined[watershed_combined$id == target_id, ]

    repeat {
      before <- nrow(selected)

      # Add all watersheds draining into the selected set
      selected <- watershed_combined[
        watershed_combined$id %in% selected$id |
          watershed_combined$to_watershed %in% selected$id,
      ]

      after <- nrow(selected)
      if (before == after) break
    }

    # Merge geometry
    merged <- sf::st_union(selected)
    merged <- nngeo::st_remove_holes(merged)
    merged_sf <- sf::st_as_sf(merged)
    merged_sf$id <- target_id

    all_results[[i]] <- merged_sf
  }

  # Bind all
  all_watershed <- do.call(rbind, all_results)

  # ------------------------------------------------------------
  # 9. Cleanup
  # ------------------------------------------------------------
  if (remove_temp) {
    cat("Removing temporary files...\n")
    file.remove(
      facc_temp,
      fdir_temp,
      pour_point_temp,
      snap_pour_point_temp,
      watershed_raster_temp,
      watershed_polygon_temp
    )
  }

  # Restore WD
  setwd(old_wd)

  # ------------------------------------------------------------
  # 10. Return final objects
  # ------------------------------------------------------------
  return(list(
    pour_point = pour_point_combined,
    watershed  = all_watershed
  ))
}
