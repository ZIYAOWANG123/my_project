#' Function `cellsize`
#'
#' This function provides a proper `cellsize` for calculation of the number of hexagons.
#'
#' @param data
#' This is input data which is a `sf` object (i.e. has geometry features).
#' @param pt
#' The minimum boundaries (both vertical and horizontal ones) of the geographical area.
#' @param nhex
#' The number of hexagons wanted/appointed by the users (for a specific region/area).
#'
#' @return A numeric number to determine the proper `cellsize` to generate the suitable number of hexagons for mapping.
#'
#' @export cellsize
#'
#'
#'

## 1. how to link the "offset" in both functions? 2. how to replace "dx" and "dy" with no "cellsize" in them?
cellsize <- function (data, pt, nhex)
{
  dx = cellsize[1]/sqrt(3)
  dy = sqrt(3) * dx/2
  bb = st_bbox(data)
  xlim = bb[c("xmin", "xmax")]
  ylim = bb[c("ymin", "ymax")]

  if (is.null(pt)) {
    pt = pt[2:1]
  }

  offset = c(x = (pt[1] - xlim[1])%%dx, y = (pt[2] - ylim[1])%%(2 * dy))

  # using formula to calculate the proper cellsize based on the # of hexagons
  ## total area and land area of the data
  area = (xlim[2] - xlim[1]) * (ylim[2] - ylim[1])
  land <- sum(st_area(data))
  ## land-area ratio
  p = land / area

  ## number of hexagons for each col and row
  nx = floor((bb[3] - offset[1])/cellsize[1])
  ny = floor((bb[4] - offset[2])/cellsize[2])
  n = nx * ny * p

  # formula
  cellsize = sqrt((area * p * 2 * sqrt(3))/ n )
}
