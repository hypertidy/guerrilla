# x is xyz matrix, triangles is cbind(.vx0, .vx1, .vx2)
mesh_triangles <- function(x, triangles, grid = NULL, n = 128) {  
  if (is.null(grid)) {
    ## set the grid extent and res (squarish pixels)
    ratio <- diff(range(x[,2L]))/diff(range(x[,1L]))
    grid <- raster::raster(raster::extent(x[,1:2]), 
                           nrows = n * ratio, ncols = n)
  }
  ## coordinates of the pixels (centres)
  rxy <- raster::xyFromCell(grid, seq_len(raster::ncell(grid)))
  ## ID for pixels + weighting per each triangle corner
  pid0 <- geometry::tsearch(x[,1L], x[,2L], triangles, rxy[, 1L], 
                            rxy[, 2L], bary = TRUE)
  ## prepare 
  ok <- !is.na(pid0$idx)
  ## zap the grid values for safety
  suppressWarnings(grid <- raster::setValues(grid, NA_real_))
  ## populate with weighted sum of relevant value at triangle corner
  value <- x[,3L]
  grid[ok] <- colSums(matrix(value[t(triangles[pid0$idx[ok], ])], nrow = 3L) * t(pid0$p)[, ok])
  
  #           colSums(matrix(value[t(tri[pid0$idx[ok], ])], nrow = 3) * t(pid0$p)[, ok])
  grid
}
.tri2quads <- quadmesh:::triangulate_quads
function(quad_index, clockwise = FALSE) {
  
  if (clockwise){
    matrix(rbind(quad_index[c(1L, 2L, 4L), ], quad_index[c(2L, 3L, 4L), ]), 3L)
  } else {
    matrix(rbind(quad_index[c(1L, 4L, 2L), ], quad_index[c(4L, 3L, 2L), ]), 3L)
  }
}
#' Mesh raster
#' 
#' Create a raster by interpolating across triangles
#' 
#' At the moment, mesh_raster is identical to [tri_fun]
#' for the matrix x-y-z case, but adds capability for 
#' a mesh3d object (of triangles). 
#' Barycentric interpolation is used to efficiently obtain
#' a within-triangle estimate of a field of values
#'
#' @param x matrix of points, or a mesh3d
#' @param grid raster to populate
#' @param n grid size of raster if 'grid' not supplied
#'
#' @return Raster
#' @export
#'
#' @examples
#' data("humface", package = "Rvcg")
#' x <- humface
#' grid <- mesh_raster(x, n = 256)
#' raster::plot(grid, col = grey.colors(21), 
#'     breaks = quantile(grid, seq(0, 1, length = 22), na.rm = TRUE))
#' anglr::plot3d(grid)
#' 
#' ## interpolate from raw points
#' xyz <- quakes[c("long", "lat", "depth")]
#' xyz$depth <- -xyz$depth
#' gx <- mesh_raster(xyz)
#' rat <- 1/cos(mean(xyz[["lat"]]) * pi/180)
#' raster::image(gx, asp = rat, 
#'   col = hcl.colors(12, "YlOrRd"))
#' maps::map(add = TRUE)
#' points(xyz, pch = "+", cex = 0.3)
#' ## add some dummy points (we aren't modelling the world)
#' 
#' xex <- cbind(expand.grid(long = range(xyz$long), 
#'                          lat = range(xyz$lat)), depth = 0)
#' g2 <- mesh_raster(rbind(xex, xyz))
#' raster::image(g2, asp = rat)
#' maps::map(add = TRUE)
#' points(xyz, pch = "+", cex = 0.3)
#' anglr::plot3d(g2); rgl::aspect3d(1, rat, 0.1)
#' rgl::points3d(xyz$long, xyz$lat, xyz$depth + 30)
mesh_raster <- function(x, grid = NULL, n = 128) {
  UseMethod("mesh_raster")
}
#' @name mesh_raster
#' @export
mesh_raster.mesh3d <- function(x, grid = NULL, n = 128) {
  pts <- t(x$vb[1:3, ])
  if (is.null(x[["it"]])) {
    x[["it"]] <- .tri2quads(x[["ib"]])
  }
  ## triangle index - rows are .vx0, .vx1, .vx2 for geometry pkg
  tri <- t(x$it)
  mesh_triangles(pts, triangles = tri, grid = grid, n = n)
}
#' @name mesh_raster
#' @export
mesh_raster.matrix <- function(x, grid = NULL, n = 128) {
  if (dim(x)[2] < 3) stop("input must be a matrix or data frame of 3 numeric columns")
  ## triangle index - rows are .vx0, .vx1, .vx2 for geometry pkg
  tri <- geometry::delaunayn(x[,1:2])
  mesh_triangles(x, triangles = tri, grid = grid, n = n)
}

#' @name mesh_raster
#' @export
mesh_raster.data.frame <- function(x, grid = NULL, n = 128) {
  if (dim(x)[2] < 3) stop("input must be a matrix or data frame of 3 numeric columns")
  mesh_raster(as.matrix(x))
}


