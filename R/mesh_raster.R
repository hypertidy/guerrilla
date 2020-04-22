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

#' Mesh raster
#' 
#' Create a raster by interpolating across triangles
#' 
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
mesh_raster <- function(x, grid = NULL, n = 128) {
  UseMethod("mesh_raster")
}
#' @name mesh_raster
#' @export
mesh_raster.mesh3d <- function(x, grid = NULL, n = 128) {
  pts <- t(x$vb[1:3, ])
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


