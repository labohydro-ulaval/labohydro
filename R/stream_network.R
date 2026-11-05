#' stream_network
#'
#' @param facc Character. Path to the D8 flow accumulation file (with extension).
#' @param fdir Character. Path to the D8 flow direction file (with extension).
#' @param output Character. Path to the stream network file (with extension).
#' @param thresholds Numeric vector. Flow accumulation thresholds used to classify streams.
#' @param working_directory Character. Working directory where temporary files are written.
#' @param remove_temp Logical. If `TRUE`, temporary files (flow_acc_reclass_temp, flow_acc_reclass_nodata_temp) are deleted.
#' @param esri_pntr Logical. If `TRUE` (default), `fdir` uses the ESRI D8 pointer scheme (as produced by `flow_dir_acc()`).
#'
#' @returns
#' stream network
#' @export
#'
#' @examples
#' # no example
stream_network <- function(facc,
                           fdir,
                           output,
                           thresholds,                   # e.g., c(5000, 10000, 20000)
                           working_directory = getwd(),
                           remove_temp = TRUE,
                           esri_pntr = TRUE) {

  # ------------------------------------------------------------
  # 1. Save and switch working directory
  # ------------------------------------------------------------
  original_wd <- getwd()
  setwd(working_directory)

  # ------------------------------------------------------------
  # 2. Temporary files
  # ------------------------------------------------------------
  reclass_file     <- "flow_acc_reclass_temp.tif"
  reclass_nodata   <- "flow_acc_reclass_nodata_temp.tif"

  # ------------------------------------------------------------
  # 3. Build reclassification triplet string
  #
  #    Example thresholds = c(5000, 10000)
  #
  #    Classes:
  #        0 : 0–5000
  #        1 : 5000–10000
  #        2 : >=10000
  #
  #    WBT expects:
  #        "0;0;5000;1;5000;10000;2;10000;9e99"
  #
  #    THIS VERSION BUILDS IT AUTOMATICALLY AND SIMPLY.
  # ------------------------------------------------------------
  thr <- sort(thresholds)
  bounds <- c(0, thr, 9e99)
  classes <- seq_along(bounds[-1]) - 1   # 0,1,2,...

  # Build triplets compactly
  reclass_vals <- paste(
    apply(
      cbind(classes, bounds[-length(bounds)], bounds[-1]),
      1,
      function(x) paste(x, collapse = ";")
    ),
    collapse = ";"
  )

  # ------------------------------------------------------------
  # 4. Reclassify flow accumulation
  # ------------------------------------------------------------
  whitebox::wbt_reclass(
    input        = facc,
    output       = reclass_file,
    reclass_vals = reclass_vals
  )

  # ------------------------------------------------------------
  # 5. Set NoData value on the reclassified raster
  #
  #    This ensures non-stream areas are properly masked.
  # ------------------------------------------------------------
  whitebox::wbt_set_nodata_value(
    input      = reclass_file,
    output     = reclass_nodata,
    back_value = 0
  )

  # ------------------------------------------------------------
  # 6. Convert raster → vector stream network
  #
  #    Whitebox will interpret:
  #       VALUE == 0 → ignored
  #       VALUE >= 1 → stream classes
  #
  #    The VALUE field will contain the multi-thresholds class.
  # ------------------------------------------------------------
  whitebox::wbt_raster_streams_to_vector(
    streams = reclass_nodata,
    d8_pntr = fdir,
    output  = output,
    esri_pntr = esri_pntr
  )

  # ------------------------------------------------------------
  # 7. Cleanup temporary files
  # ------------------------------------------------------------
  if (remove_temp) {
    temp_files <- c(reclass_file, reclass_nodata)
    idx <- file.exists(temp_files)
    if (any(idx)) file.remove(temp_files[idx])
  }

  # ------------------------------------------------------------
  # 8. Restore original working directory
  # ------------------------------------------------------------
  setwd(original_wd)
}
