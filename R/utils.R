#' Title
#'
#' @param xy coordinates
#' @param ncols number of columns
#' @param nrows number of rows
#' @param prj projection metadata
#'
#' @return raster
#' @export
defaultgrid <- function(xy, ncols = 60, nrows = 50, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") {
  raster::raster(raster::extent(xy), ncols = ncols, nrows = ncols, crs = prj)
}