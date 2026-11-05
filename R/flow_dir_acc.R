#' flow_dir_acc
#'
#' @param input Character. Path to the digital elevation model file (with extension).
#' @param output Character. Path to the flow accumulation file (with extension).
#' @param working_directory Character. Working directory where temporary files are written.
#' @param remove_flowdir Logical. If `TRUE`, the flow direction file (flow_dir) is deleted.
#' @param esri_pntr Logical. If `TRUE` (default), the D8 flow direction uses the ESRI pointer scheme.
#'
#' @returns
#' flow dir and acc
#' @export
#'
#' @examples
#' # no example
flow_dir_acc <- function(input,
                         output = "flow_acc.tif",
                         working_directory = getwd(),
                         remove_flowdir = TRUE,
                         esri_pntr = TRUE) {

  # ------------------------------------------------------------
  # 1. Save and switch working directory
  # ------------------------------------------------------------
  original_wd <- getwd()
  setwd(working_directory)

  # ------------------------------------------------------------
  # 2. Define temporary flow-direction raster name
  #
  #    This ensures:
  #    - clear file naming
  #    - predictable location
  #    - easy cleanup later
  # ------------------------------------------------------------
  flowdir_file <- "flow_dir.tif"

  # ------------------------------------------------------------
  # 3. Compute D8 flow direction raster
  #
  #    WhiteboxTools creates a pointer raster indicating
  #    the direction water flows out of each pixel.
  #
  #    `esri_pntr = TRUE` (default) ensures ESRI-style pointer encoding.
  # ------------------------------------------------------------
  whitebox::wbt_d8_pointer(
    dem   = input,
    output = flowdir_file,
    esri_pntr = esri_pntr
  )

  # ------------------------------------------------------------
  # 4. Compute flow accumulation raster
  #
  #    Uses the D8 flow direction raster as the input.
  #    Output is written directly to the user-provided path.
  # ------------------------------------------------------------
  whitebox::wbt_d8_flow_accumulation(
    input = flowdir_file,
    output = output,
    pntr = TRUE,
    esri_pntr = esri_pntr
  )

  # ------------------------------------------------------------
  # 5. Optional removal of temporary flow direction raster
  #
  #    This keeps the working directory clean unless the
  #    user wants to keep the flow_dir raster for inspection.
  # ------------------------------------------------------------
  if (remove_flowdir) {
    if (file.exists(flowdir_file)) {
      file.remove(flowdir_file)
    }
  }

  # ------------------------------------------------------------
  # 6. Restore the original working directory
  # ------------------------------------------------------------
  setwd(original_wd)
}
