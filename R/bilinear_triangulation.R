#' Interpolation to a regular grid via triangulation
#'
#' Triangulate the input coordinates and estimate a value at every cell of a
#' target grid, using the barycentric coordinates of the cell centre within the
#' triangle that contains it.
#'
#' Cells that fall outside the convex hull of `xy` have no containing triangle
#' and are left as `NA`.
#'
#' @param xy coordinates
#' @param value value to interpolate
#' @param grid grid to use
#' @param ... ignored
#'
#' @return raster
#' @export
#' @examples
#' zero_extent <- raster::extent(0, ncol(volcano), 0, nrow(volcano))
#' r <- raster::setExtent(raster::raster(volcano), zero_extent)
#' xy <- raster::sampleRandom(r, size = 150, xy = TRUE)[, 1:2, drop = FALSE]
#' tri_est <- tri_fun(xy, raster::extract(r, xy))
#'
#' grd <- raster::raster(raster::extent(xy) ,res = 0.1)
#' tri_est2 <- tri_fun(xy, raster::extract(r, xy), grid = grd)
tri_fun <- function(xy, value, grid = NULL, ...) {
  if (is.null(grid)) grid <- defaultgrid(xy)
  tri <- geometry::delaunayn(xy); 
  rxy <- raster::xyFromCell(grid, seq_len(raster::ncell(grid)))
  # this is calculating barycentric weights for a grid of points (rxy - grid but can be arbitrary) 
  # across a triangle mesh
  pid0 <- geometry::tsearch(xy[,1], xy[,2], tri, rxy[,1], rxy[, 2],
                            bary = TRUE)
  ok <- !is.na(pid0$idx)
  ## because min() is giving warnings, check raster
  suppressWarnings( grid <- raster::setValues(grid, NA_real_))
  # then calculating the weighted interpolated value for each, and then updating the grid structure's values
  grid[ok] <- colSums(matrix(value[t(tri[pid0$idx[ok], ])], nrow = 3) * t(pid0$p)[, ok])
  grid
}
