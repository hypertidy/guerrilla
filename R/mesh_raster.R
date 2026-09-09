.tri2quads <-
function(quad_index, clockwise = FALSE) {

  if (clockwise){
    matrix(rbind(quad_index[c(1L, 2L, 4L), ], quad_index[c(2L, 3L, 4L), ]), 3L)
  } else {
    matrix(rbind(quad_index[c(1L, 4L, 2L), ], quad_index[c(4L, 3L, 2L), ]), 3L)
  }
}

## A grid of about n cells across, with squarish cells, over these coordinates.
grid_for <- function(xy, n = 128) {
  ratio <- diff(range(xy[, 2L])) / diff(range(xy[, 1L]))
  grid_spec(xy, dimension = c(n, max(1, round(n * ratio))))
}

#' Mesh raster
#'
#' Create a grid by interpolating across triangles.
#'
#' For x-y-z input this is [grid_barycentric()] with the arguments arranged
#' differently:
#' triangulate, then estimate each cell from the triangle containing it. The
#' difference is that `mesh_raster()` also takes a `mesh3d`, where the triangles
#' already exist and no triangulation is needed.
#'
#' That case is worth having because it runs the argument backwards. A grid is
#' one particular mesh -- a regular one, with the values at the vertices -- so
#' turning an arbitrary mesh into a grid is resampling one mesh onto another,
#' and barycentric interpolation is how you do it either way.
#'
#' @param x a matrix or data frame of three columns, or a `mesh3d`
#' @param grid a [grid_spec()] to interpolate onto, or `NULL` for a default one
#' @param n cells across, when `grid` is not supplied
#' @param ... passed to methods, and on to [grid_barycentric()]
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' ## interpolate from raw points
#' xyz <- quakes[c("long", "lat", "depth")]
#' xyz$depth <- -xyz$depth
#' gx <- mesh_raster(xyz)
#' rat <- 1/cos(mean(xyz[["lat"]]) * pi/180)
#' plot(gx, asp = rat, col = hcl.colors(12, "YlOrRd"))
#' points(xyz, pch = "+", cex = 0.3)
#'
#' ## add some dummy points (we aren't modelling the world)
#' xex <- cbind(expand.grid(long = range(xyz$long),
#'                          lat = range(xyz$lat)), depth = 0)
#' g2 <- mesh_raster(rbind(xex, xyz))
#' plot(g2, asp = rat)
#' points(xyz, pch = "+", cex = 0.3)
#'
#' if (requireNamespace("maps", quietly = TRUE)) {
#'   maps::map(add = TRUE)
#' }
#'
#' ## a mesh of triangles, rather than raw points
#' if (requireNamespace("Rvcg", quietly = TRUE)) {
#'   data("humface", package = "Rvcg")
#'   grid <- mesh_raster(humface, n = 256)
#'   plot(grid, col = grey.colors(24))
#' }
#'
#' ## and back the other way, in 3D
#' if (interactive() && requireNamespace("anglr", quietly = TRUE)) {
#'   anglr::plot3d(as_raster(g2))
#'   rgl::aspect3d(1, rat, 0.1)
#'   rgl::points3d(xyz$long, xyz$lat, xyz$depth + 30)
#' }
mesh_raster <- function(x, grid = NULL, n = 128, ...) {
  UseMethod("mesh_raster")
}
#' @name mesh_raster
#' @export
mesh_raster.mesh3d <- function(x, grid = NULL, n = 128, ...) {
  xyz <- t(x$vb[1:3, ])
  if (is.null(x[["it"]])) {
    x[["it"]] <- .tri2quads(x[["ib"]])
  }
  ## triangle index - rows are .vx0, .vx1, .vx2 for geometry pkg
  triangles <- t(x$it)
  if (is.null(grid)) grid <- grid_for(xyz, n) else grid <- as_grid(grid)
  ## the triangles already exist, so there is nothing to triangulate: this is
  ## the second half of grid_barycentric() on its own.
  grid$values <- bary_interpolate_geometry(xyz[, 1:2, drop = FALSE], xyz[, 3L],
                                           triangles, grid_xy(grid))
  grid
}
#' @name mesh_raster
#' @export
mesh_raster.matrix <- function(x, grid = NULL, n = 128, ...) {
  xyz <- as_xy(x, 3L)
  if (is.null(grid)) grid <- grid_for(xyz, n)
  grid_barycentric(xyz[, 1:2, drop = FALSE], xyz[, 3L], grid = grid, ...)
}

#' @name mesh_raster
#' @export
mesh_raster.data.frame <- function(x, grid = NULL, n = 128, ...) {
  mesh_raster(as.matrix(x), grid = grid, n = n, ...)
}
