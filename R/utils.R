#' Title
#'
#' @param xy coordinates
#' @param ncol number of columns
#' @param nrow number of rows
#' @param prj projection metadata
#'
#' @return raster
#' @export
defaultgrid <- function(xy, ncol = 60, nrow = 50, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") {
  raster::raster(raster::extent(xy), ncol = 60, nrow = 50, crs = prj)
}