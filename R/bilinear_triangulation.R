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

#' Interpolate to a grid across a triangulation
#'
#' Triangulate the input coordinates, then estimate a value at every cell of a
#' target grid from the barycentric coordinates of the cell centre within the
#' triangle that contains it.
#'
#' Within a triangle the surface is the plane through its three corner values,
#' so the result is continuous, passes exactly through the input values, and
#' invents nothing beyond them. This is what MATLAB calls
#' `griddata(method = "linear")` and what GDAL's `gdal_grid` calls `linear`.
#'
#' Cells outside the convex hull of the points are in no triangle, and are left
#' `NA`. Points sharing a coordinate are combined first, because a triangulation
#' cannot hold two values in one place; see [collapse_duplicates()].
#'
#' @section Two engines:
#' `engine = "geometry"` locates the cells and computes their weights in one
#' pass of `geometry::tsearch()`, in C. It is the one to use.
#'
#' `engine = "R"` does the same work in the open: [find_triangle()] to locate
#' each cell, [bary_weights()] to weight it. It is slower, and it is the point
#' of this package -- the two agree to floating point, and the second one can be
#' read.
#'
#' @param x coordinates, or coordinates carrying their value as z; see
#'   [xyz_input()]
#' @param value one value per coordinate, or `NULL` to use the z of `x`
#' @param grid a [grid_spec()] to interpolate onto, or `NULL` for a default one
#' @param duplicates function used to combine values at repeated coordinates,
#'   or `NULL` to leave them alone
#' @param engine `"geometry"` for the fast path, `"R"` for the readable one
#' @param ... ignored
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(100), runif(100))
#' grid_barycentric(xy, xy[, 1] + xy[, 2])
#'
#' ## the value can be the z of the coordinates
#' grid_barycentric(cbind(xy, xy[, 1]))
#'
#' ## the two engines agree
#' g1 <- grid_barycentric(xy, xy[, 1], grid_spec(xy, dimension = c(20, 20)))
#' g2 <- grid_barycentric(xy, xy[, 1], grid_spec(xy, dimension = c(20, 20)),
#'                        engine = "R")
#' max(abs(g1$values - g2$values), na.rm = TRUE)
grid_barycentric <- function(x, value = NULL, grid = NULL, duplicates = mean,
                             engine = c("geometry", "R"), ...) {
  engine <- match.arg(engine)
  input <- xyz_input(x, value)
  xy <- input$xy
  value <- input$value
  if (!is.null(duplicates)) {
    dd <- collapse_duplicates(xy, value, duplicates,
                              fun_label = deparse(substitute(duplicates)))
    xy <- dd$xy
    value <- dd$value
  }
  if (is.null(grid)) {
    grid <- grid_spec(xy, crs = input$crs)
  } else {
    grid <- as_grid(grid)
  }
  triangles <- delaunay_index(xy)
  rxy <- grid_xy(grid)
  grid$values <- if (engine == "geometry") {
    bary_interpolate_geometry(xy, value, triangles, rxy)
  } else {
    bary_interpolate_r(xy, value, triangles, rxy)
  }
  grid
}

## tsearch() finds the containing triangle and its barycentric weights in one
## C pass; the estimate is the weighted sum of that triangle's corner values.
bary_interpolate_geometry <- function(xy, value, triangles, points) {
  hit <- geometry::tsearch(xy[, 1L], xy[, 2L], triangles,
                           points[, 1L], points[, 2L], bary = TRUE)
  ok <- !is.na(hit$idx)
  out <- rep(NA_real_, nrow(points))
  out[ok] <- colSums(matrix(value[t(triangles[hit$idx[ok], ])], nrow = 3L) *
                       t(hit$p)[, ok])
  out
}

## The same thing, spelled out: locate, weight, combine.
bary_interpolate_r <- function(xy, value, triangles, points) {
  idx <- find_triangle(xy, triangles, points)
  out <- rep(NA_real_, nrow(points))
  for (i in which(!is.na(idx))) {
    corners <- triangles[idx[i], ]
    w <- bary_weights(xy[corners, , drop = FALSE], points[i, , drop = FALSE])
    out[i] <- sum(w[1L, ] * value[corners])
  }
  out
}

#' Interpolation to a regular grid via triangulation
#'
#' Superseded by [grid_barycentric()], which does the same thing under a name
#' that says what it is.
#'
#' @param xy coordinates
#' @param value value to interpolate
#' @param grid grid to use
#' @param duplicates passed to [grid_barycentric()]
#' @param ... passed to [grid_barycentric()]
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @keywords internal
#' @examples
#' xy <- cbind(runif(50), runif(50))
#' grid_barycentric(xy, xy[, 1])  ## use this instead
tri_fun <- function(xy, value, grid = NULL, duplicates = mean, ...) {
  .Deprecated("grid_barycentric")
  grid_barycentric(xy, value, grid = grid, duplicates = duplicates, ...)
}
