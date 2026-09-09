#' Create a target grid for interpolation
#'
#' Build a raster grid covering the extent of a set of coordinates, to use as
#' the target for the interpolation functions in this package.
#'
#' The grid carries no coordinate reference system unless one is given. Nothing
#' in this package requires the input to be longitude/latitude, or geographic at
#' all, so asserting a CRS by default would be asserting something untrue for
#' most inputs. Pass `prj` explicitly when the coordinates really are in a known
#' system.
#'
#' The grid covers the exact extent of `xy`, so the outermost input coordinates
#' fall on the boundary of the grid.
#'
#' @param xy coordinates
#' @param ncols number of columns
#' @param nrows number of rows
#' @param prj projection metadata, `NA` (the default) for none
#'
#' @return raster
#' @export
#' @examples
#' xy <- cbind(c(0, 4, 10), c(0, 2, 5))
#' defaultgrid(xy)
#'
#' ## with a CRS, when the coordinates really are longitude/latitude
#' lonlat <- cbind(c(100, 130, 160), c(-70, -55, -40))
#' defaultgrid(lonlat, prj = "EPSG:4326")
defaultgrid <- function(xy, ncols = 60, nrows = 50, prj = NA) {
  raster::raster(raster::extent(xy), ncols = ncols, nrows = nrows, crs = prj)
}
