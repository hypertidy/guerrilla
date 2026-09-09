#' Barycentric coordinates of points within a triangle
#'
#' The weights that say how much of each corner a point is made of. They sum to
#' one, and they are all non-negative exactly when the point is inside the
#' triangle -- which makes them a point-in-triangle test and an interpolation
#' rule at the same time.
#'
#' This is the whole of what `geometry::tsearch(bary = TRUE)` computes in C,
#' written out so it can be read. For a triangle with corners
#' \eqn{(x_1,y_1)}, \eqn{(x_2,y_2)}, \eqn{(x_3,y_3)} and a point
#' \eqn{(x,y)}, solve for the weights by Cramer's rule on
#'
#' \deqn{w_1 (x_1,y_1) + w_2 (x_2,y_2) + w_3 (x_3,y_3) = (x,y), \quad
#'       w_1 + w_2 + w_3 = 1}
#'
#' Estimating a value is then one line: the weighted sum of the corner values.
#'
#' @param triangle three corners, as a 3 by 2 matrix
#' @param xy points to place within it, a two column matrix
#'
#' @return A matrix with one row per point and three columns, one weight per
#'   corner of `triangle`.
#' @export
#' @seealso [find_triangle()], which uses this to locate points, and
#'   [grid_barycentric()], which uses it to interpolate.
#' @examples
#' tri <- cbind(c(0, 1, 0), c(0, 0, 1))
#' ## the corners themselves
#' bary_weights(tri, tri)
#'
#' ## the centroid is one third of each
#' bary_weights(tri, cbind(1/3, 1/3))
#'
#' ## outside the triangle, a weight goes negative
#' bary_weights(tri, cbind(1, 1))
bary_weights <- function(triangle, xy) {
  triangle <- as_xy(triangle)
  xy <- as_xy(xy)
  stopifnot(nrow(triangle) == 3L)
  x1 <- triangle[1L, 1L]; y1 <- triangle[1L, 2L]
  x2 <- triangle[2L, 1L]; y2 <- triangle[2L, 2L]
  x3 <- triangle[3L, 1L]; y3 <- triangle[3L, 2L]
  ## twice the signed area of the triangle; zero if the corners are collinear
  det <- (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
  px <- xy[, 1L] - x3
  py <- xy[, 2L] - y3
  w1 <- ((y2 - y3) * px + (x3 - x2) * py) / det
  w2 <- ((y3 - y1) * px + (x1 - x3) * py) / det
  cbind(w1, w2, 1 - w1 - w2, deparse.level = 0L)
}

#' Which triangle is each point in?
#'
#' For every point, the row of `triangles` whose triangle contains it, or `NA`
#' for a point in none of them.
#'
#' This is what the old `tri_pip()` did by building one \pkg{sp} polygon per
#' triangle and calling `over()`. GEOS does the same job with a spatial index it
#' builds for you: [geos::geos_intersects_matrix()] puts the triangles in an
#' STRtree, so each point is only tested against the few whose bounding boxes it
#' falls in. The exact containment test is then [bary_weights()], in R.
#'
#' @param xy vertex coordinates, a two column matrix
#' @param triangles one triangle per row, as three indices into `xy`
#' @param points points to locate, a two column matrix
#'
#' @return An integer vector with one element per row of `points`.
#' @export
#' @examples
#' xy <- cbind(c(0, 1, 1, 0), c(0, 0, 1, 1))
#' triangles <- rbind(c(1, 2, 3), c(1, 3, 4))
#' find_triangle(xy, triangles, cbind(c(0.9, 0.1, 5), c(0.5, 0.9, 5)))
find_triangle <- function(xy, triangles, points) {
  if (!requireNamespace("geos", quietly = TRUE)) {
    stop("find_triangle() needs the geos package")
  }
  xy <- as_xy(xy)
  points <- as_xy(points)
  tri_geom <- triangle_geometry(xy, triangles)
  ## geos indexes the second argument, so each point is compared with only the
  ## triangles whose envelope it falls inside
  cand <- geos::geos_intersects_matrix(
    geos::as_geos_geometry(wk::xy(points[, 1L], points[, 2L])), tri_geom)
  out <- rep(NA_integer_, nrow(points))
  for (i in seq_along(cand)) {
    for (k in as.integer(cand[[i]])) {
      w <- bary_weights(xy[triangles[k, ], , drop = FALSE],
                        points[i, , drop = FALSE])
      if (!anyNA(w) && all(w >= -1e-12)) {
        out[i] <- k
        break
      }
    }
  }
  out
}

## One closed polygon per triangle, as a geos geometry vector.
triangle_geometry <- function(xy, triangles) {
  idx <- triangles[, c(1L, 2L, 3L, 1L), drop = FALSE]
  n <- nrow(triangles)
  geos::as_geos_geometry(
    wk::wkt(sprintf("POLYGON ((%s))",
                    vapply(seq_len(n), function(i) {
                      v <- xy[idx[i, ], , drop = FALSE]
                      paste(sprintf("%.17g %.17g", v[, 1L], v[, 2L]),
                            collapse = ", ")
                    }, character(1L)))))
}
