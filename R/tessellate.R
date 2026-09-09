## Voronoi tiles for a set of points, and the point each tile belongs to.
##
## GEOS returns the tiles in its own order, not the order of the points that
## generated them, so the mapping has to be recovered. Each point lies in
## exactly one tile, which is what makes that easy and also what makes the
## tessellation worth drawing.
voronoi_tiles <- function(xy) {
  if (!requireNamespace("geos", quietly = TRUE)) {
    stop("this needs the geos package")
  }
  pts <- geos::as_geos_geometry(wk::xy(xy[, 1L], xy[, 2L]))
  tiles <- geos::geos_unnest(
    geos::geos_voronoi_polygons(geos::geos_make_collection(pts)),
    keep_multi = FALSE)
  owner <- vapply(geos::geos_intersects_matrix(pts, tiles),
                  function(i) if (length(i)) as.integer(i[1L]) else NA_integer_,
                  integer(1L))
  list(tiles = tiles, owner = owner, points = pts)
}

#' Nearest neighbour interpolation, by Voronoi tessellation
#'
#' Give every cell the value of the closest input point.
#'
#' The tessellation is the picture of that rule. A Voronoi tile is exactly the
#' region closer to its own point than to any other, so assigning each cell to
#' the tile it falls in is nearest neighbour, drawn rather than computed.
#'
#' Unlike [grid_barycentric()], this fills the whole grid. Voronoi tiles cover
#' the plane, so there is no convex hull to fall outside of -- which is
#' convenient and is also the method's main way of lying to you, since a cell
#' far beyond the data gets a confident answer from one distant point.
#'
#' @inheritParams grid_barycentric
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @examples
#' xy <- cbind(runif(30), runif(30))
#' plot(grid_voronoi(xy, xy[, 1]))
#' points(xy, pch = 16, cex = 0.5)
grid_voronoi <- function(x, value = NULL, grid = NULL, duplicates = mean, ...) {
  input <- xyz_input(x, value)
  xy <- input$xy
  value <- input$value
  if (!is.null(duplicates)) {
    dd <- collapse_duplicates(xy, value, duplicates,
                              fun_label = deparse(substitute(duplicates)))
    xy <- dd$xy
    value <- dd$value
  }
  if (is.null(grid)) grid <- grid_spec(xy, crs = input$crs) else grid <- as_grid(grid)
  vt <- voronoi_tiles(xy)
  rxy <- grid_xy(grid)
  cells <- geos::as_geos_geometry(wk::xy(rxy[, 1L], rxy[, 2L]))
  hit <- geos::geos_intersects_matrix(cells, vt$tiles)
  tile <- vapply(hit, function(i) if (length(i)) as.integer(i[1L]) else NA_integer_,
                 integer(1L))
  ## tile -> the point that generated it -> that point's value
  point_of_tile <- match(seq_along(vt$tiles), vt$owner)
  grid$values <- value[point_of_tile[tile]]
  grid
}

#' Fit a plane within each Delaunay triangle
#'
#' Triangulate the points, fit `value ~ x + y` by least squares to the three
#' corners of each triangle, and evaluate that fit at the cells inside it.
#'
#' This is the long way round to [grid_barycentric()], and the two agree to
#' floating point. Three points determine a plane, least squares through three
#' points is that plane, and barycentric interpolation evaluates that same
#' plane -- so the expensive per-triangle `lm()` and the one vectorised
#' `tsearch()` call compute the same surface. Worth seeing once.
#'
#' It replaces the Delaunay half of `facets()`, which did this through
#' \pkg{spatstat} and returned a point pattern rather than a grid.
#'
#' The Voronoi half of `facets()` is not here, because it was not doing what it
#' looked like it was doing. A Voronoi tile contains exactly one point by
#' construction, so `lm(value ~ x + y)` on it fits an intercept and nothing
#' else, and predicts that one point's value across the whole tile. That is
#' nearest neighbour, computed the slowest way available. [grid_voronoi()] is
#' the same answer under its right name.
#'
#' @inheritParams grid_barycentric
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @seealso [grid_barycentric()] for the fast form of this, [grid_voronoi()]
#'   for what the Voronoi case really was.
#' @examples
#' xy <- cbind(runif(40), runif(40))
#' v <- xy[, 1] + xy[, 2]
#' plot(grid_facet_lm(xy, v))
grid_facet_lm <- function(x, value = NULL, grid = NULL,
                          duplicates = mean, ...) {
  if (!requireNamespace("geos", quietly = TRUE)) {
    stop("grid_facet_lm() needs the geos package")
  }
  input <- xyz_input(x, value)
  xy <- input$xy
  value <- input$value
  if (!is.null(duplicates)) {
    dd <- collapse_duplicates(xy, value, duplicates,
                              fun_label = deparse(substitute(duplicates)))
    xy <- dd$xy
    value <- dd$value
  }
  if (is.null(grid)) grid <- grid_spec(xy, crs = input$crs) else grid <- as_grid(grid)

  pts <- geos::as_geos_geometry(wk::xy(xy[, 1L], xy[, 2L]))
  facets <- geos::geos_unnest(
    geos::geos_delaunay_triangles(geos::geos_make_collection(pts)),
    keep_multi = FALSE)
  rxy <- grid_xy(grid)
  cells <- geos::as_geos_geometry(wk::xy(rxy[, 1L], rxy[, 2L]))
  ## which input points, and which cells, fall in each facet
  in_facet <- geos::geos_intersects_matrix(pts, facets)
  cell_facet <- geos::geos_intersects_matrix(cells, facets)

  members <- split(rep(seq_along(in_facet), lengths(in_facet)),
                   factor(unlist(in_facet), levels = seq_along(facets)))
  out <- rep(NA_real_, nrow(rxy))
  for (f in seq_along(facets)) {
    who <- members[[f]]
    if (length(who) < 3L) next
    cells_here <- which(vapply(cell_facet, function(i) f %in% i, logical(1L)))
    if (!length(cells_here)) next
    d <- data.frame(value = value[who], x = xy[who, 1L], y = xy[who, 2L])
    fit <- stats::lm(value ~ x + y, data = d)
    out[cells_here] <- stats::predict(
      fit, data.frame(x = rxy[cells_here, 1L], y = rxy[cells_here, 2L]))
  }
  grid$values <- out
  grid
}
