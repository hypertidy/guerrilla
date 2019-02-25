
#' Identify point-in-triangle by conversion to polygons
#' 
#' This functio was used by an early version of `tri_fun`. 
#' @param tri list P n*2 coordinates and T matrix of n*3 indices defining triangles
#' @param pts input points
#' @importFrom sp Polygon Polygons SpatialPolygons CRS proj4string over
tri_pip <- function(tri, pts) {
  ps <- lapply(split(tri$T, seq(nrow(tri$T))), function(x) Polygon(tri$P[c(x, x[1]), ]))
  sp <- lapply(seq_along(ps), function(x) Polygons(ps[x], x))
  spp <- SpatialPolygons(sp, proj4string = CRS(proj4string(pts)))
  as.integer(sp::over(pts, spp))
}



#' Interpolation to a regular grid via triangulation
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
  rxy <- sp::coordinates(grid)
  pid0 <- geometry::tsearch(xy[,1], xy[,2], tri, rxy[,1], rxy[, 2],
                            bary = TRUE)
  ok <- !is.na(pid0$idx)
  r <- raster::setValues(grid, NA_real_)
  r[ok] <- colSums(matrix(value[t(tri[pid0$idx[ok], ])], nrow = 3) * t(pid0$p)[, ok])
  r 
}