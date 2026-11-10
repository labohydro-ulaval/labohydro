#' thin_points
#'
#' Randomly thins points within each group so that no two retained points are
#' closer than `radius` metres. Points are shuffled, then each retained point
#' removes all the other points of its group located within `radius`.
#'
#' @param data An `sf` object. Points to thin.
#' @param group Character. Name(s) of the column(s) of `data` defining the groups,
#'   e.g. `"milieu"` or `c("milieu", "annee")`. With several columns, each
#'   combination of values is a group. The distance rule is applied separately
#'   within each group. Default `NULL` (all points are treated as a single group).
#' @param radius Numeric. Minimum distance, in metres, between two retained points
#'   of the same group.
#' @param seed Integer. Random seed for reproducibility. Default `1`.
#'
#' @returns
#' An `sf` object with the retained points, in their original order.
#' @export
#'
#' @examples
#' # no example
thin_points <- function(data,
                        group = NULL,
                        radius,
                        seed = 1) {

  if (!inherits(data, "sf")) {
    stop("data must be an sf object.")
  }

  if (!is.null(group) && !all(group %in% names(data))) {
    stop("group must contain names of columns of data.")
  }

  set.seed(seed)

  # Row indices of each group, one group per combination of values of the
  # group columns (NA is kept as its own value)
  if (is.null(group)) {
    groups <- list(seq_len(nrow(data)))
  } else {
    factors <- lapply(group, function(col) factor(data[[col]], exclude = NULL))
    groups <- split(seq_len(nrow(data)), factors, drop = TRUE)
  }

  kept <- integer(0)

  for (idx in groups) {

    # Shuffle points so the retained points are chosen randomly
    idx <- idx[sample.int(length(idx))]

    distances <- as.numeric(sf::st_distance(data[idx, ]))
    distances <- matrix(distances, nrow = length(idx))

    keep <- rep(TRUE, length(idx))

    for (i in seq_along(idx)) {
      if (keep[i]) {
        # Remove all points closer than radius to the retained point i
        neighbours <- which(distances[i, ] < radius)
        keep[neighbours] <- FALSE
        keep[i] <- TRUE
      }
    }

    kept <- c(kept, idx[keep])
  }

  return(data[sort(kept), ])
}
