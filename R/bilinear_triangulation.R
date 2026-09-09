## The engine behind tri_fun() and mesh_raster(): given points with a value,
## a triangulation of them, and a grid, estimate the value at every cell.
##
## geometry::tsearch() does two jobs at once, which is why this is short and
## fast. For each cell centre it finds the triangle containing it, and returns
## that point's barycentric coordinates within the triangle -- three weights
## summing to one, saying how much of each corner the point is made of. The
## estimate is then the weighted sum of the corner values.
interpolate_triangles <- function(xyz, triangles, grid) {
  rxy <- grid_xy(grid)
  hit <- geometry::tsearch(xyz[, 1L], xyz[, 2L], triangles,
                           rxy[, 1L], rxy[, 2L], bary = TRUE)
  ## cells outside the convex hull are in no triangle at all
  ok <- !is.na(hit$idx)
  value <- xyz[, 3L]
  values <- rep(NA_real_, nrow(rxy))
  values[ok] <- colSums(matrix(value[t(triangles[hit$idx[ok], ])], nrow = 3L) *
                          t(hit$p)[, ok])
  grid$values <- values
  grid
}

## Delaunay triangulation, with an error a reader can act on.
##
## Two ways this fails, and they look nothing alike: too few points makes Qhull
## raise an error, while points that are all on one line make it return zero
## triangles and only warn. Both mean the same thing here.
delaunay_index <- function(xy) {
  ## Qhull's warnings are left alone on purpose: "n points missing from
  ## triangulation" is how you find out that duplicates = NULL silently
  ## dropped your data.
  tri <- tryCatch(geometry::delaunayn(xy), error = function(e) {
    stop(no_triangles("Qhull said: ", conditionMessage(e)), call. = FALSE)
  })
  if (is.null(tri) || NROW(tri) < 1L) {
    stop(no_triangles("Qhull returned no triangles."), call. = FALSE)
  }
  tri
}

no_triangles <- function(...) {
  paste0("could not triangulate these ", "coordinates.\n  ", ...,
         "\n  This means the points are all on one line, or there are too few ",
         "of them\n  (three is the minimum, and they must not be collinear).",
         "\n  Repeated coordinates are handled by the 'duplicates' argument; ",
         "collinear ones cannot be.")
}

#' Interpolation to a regular grid via triangulation
#'
#' Triangulate the input coordinates and estimate a value at every cell of a
#' target grid, from the barycentric coordinates of the cell centre within the
#' triangle that contains it.
#'
#' This is the same thing MATLAB calls `griddata(method = "linear")`, and the
#' same thing GDAL's `gdal_grid` calls the `linear` algorithm. Within a triangle
#' the surface is a plane through the three corner values, so the result is
#' continuous, passes exactly through the input values, and invents nothing
#' beyond them.
#'
#' Cells outside the convex hull of `xy` are in no triangle and are left `NA`.
#' Points at the same coordinate are collapsed first, because a triangulation
#' cannot represent two values in one place; see [collapse_duplicates()].
#'
#' @param xy coordinates
#' @param value one value per row of `xy`
#' @param grid a [grid_spec()] to interpolate onto, or `NULL` for a default one
#' @param duplicates function used to combine values at repeated coordinates,
#'   or `NULL` to leave them alone and let the triangulation fail
#' @param ... ignored
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(100), runif(100))
#' tri_fun(xy, xy[, 1] + xy[, 2])
#'
#' ## onto a grid you chose
#' tri_fun(xy, xy[, 1], grid_spec(xy, dimension = c(20, 20), pad = 0.1))
tri_fun <- function(xy, value, grid = NULL, duplicates = mean, ...) {
  xy <- as_xy(xy)
  if (length(value) != nrow(xy)) {
    stop("'value' must have one element per row of 'xy': ",
         length(value), " values for ", nrow(xy), " coordinates")
  }
  if (!is.null(duplicates)) {
    dd <- collapse_duplicates(xy, value, duplicates,
                              fun_label = deparse(substitute(duplicates)))
    xy <- dd$xy
    value <- dd$value
  }
  if (is.null(grid)) grid <- grid_spec(xy) else grid <- as_grid(grid)
  interpolate_triangles(cbind(xy, value), delaunay_index(xy), grid)
}
