#' Interpolate with GDAL's own gridder
#'
#' Hand the points to GDAL and let `gdal_grid` do the interpolation, returning
#' the result as a grid like everything else here.
#'
#' Four of the methods in this package have a `gdal_grid` twin, and running
#' them side by side is the best available check that any of them is right.
#' They are independent implementations in different languages by different
#' people, so agreement is evidence and disagreement is a bug in one of them:
#'
#' \tabular{ll}{
#'   `"linear:radius=0.0"`  \tab [grid_barycentric()] \cr
#'   `"nearest"`            \tab [grid_voronoi()] \cr
#'   `"invdist:power=2.0"`  \tab [grid_idw()] \cr
#'   `"average:radius1=,radius2="` \tab [grid_bin()], with a search radius \cr
#' }
#'
#' @section Two GDAL defaults worth knowing:
#' `algorithm = "linear"` on its own does not stop at the convex hull. GDAL's
#' default is `radius=-1`, an infinite search, so a cell in no triangle takes
#' the value of the nearest point instead of being left out. That is
#' extrapolation, it is silent, and it is why `"linear:radius=0.0"` rather than
#' `"linear"` is the one that matches [grid_barycentric()].
#'
#' Second, `gdal_grid` fills cells it could not estimate with 0 and does not
#' record that 0 as the band's no data value, so nothing downstream can tell it
#' apart from a real measurement -- and 0 is a real temperature, a real
#' elevation and a real anomaly. This function therefore appends `nodata=nan`
#' unless the algorithm string already sets one. That both fills the empty
#' cells with `NaN` and tags the band, and \pkg{gdalraster} turns the tagged
#' value into `NA` on the way back, so an explicit `nodata=-9999` arrives here
#' as `NA` just the same.
#'
#' @section Why sf:
#' `gdal_grid` is reachable from R only through [sf::gdal_utils()].
#' \pkg{gdalraster} wraps `warp`, `translate` and `rasterize` but not
#' `GDALGrid`, and the unified `gdal` command line added in GDAL 3.11 has no
#' grid subcommand either, so there is nothing for it to wrap yet.
#'
#' @inheritParams grid_barycentric
#' @param algorithm a `gdal_grid` algorithm string, passed to `-a`
#' @param nodata value GDAL should write for empty cells, read back as `NA`;
#'   ignored if `algorithm` already sets one
#' @param ... further arguments appended to the `gdal_grid` command line
#'
#' @return A `guerrilla_grid` with values.
#' @export
#' @seealso [grid_barycentric()], [grid_voronoi()], [grid_idw()]
#' @examples
#' xy <- cbind(runif(50), runif(50))
#' if (requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("gdalraster", quietly = TRUE)) {
#'   g <- grid_spec(xy, dimension = c(40, 40))
#'   gdal <- grid_gdal(xy, xy[, 1], g, algorithm = "linear:radius=0.0")
#'   ours <- grid_barycentric(xy, xy[, 1], g)
#'   max(abs(gdal$values - ours$values), na.rm = TRUE)
#' }
grid_gdal <- function(x, value = NULL, grid = NULL, algorithm = "linear",
                      nodata = NaN, ...) {
  need_package("sf", "grid_gdal()")
  need_package("gdalraster", "grid_gdal()")
  s <- method_setup(x, value, grid)
  if (!grepl("nodata=", algorithm, fixed = TRUE)) {
    algorithm <- paste0(algorithm, ":nodata=",
                        if (is.nan(nodata)) "nan" else format(nodata))
  }
  src <- tempfile(fileext = ".gpkg")
  dst <- tempfile(fileext = ".tif")
  on.exit(unlink(c(src, dst)), add = TRUE)

  ## gdal_grid works in whatever coordinates it is given, and -txe/-tye are in
  ## those same coordinates, so the crs changes nothing about the answer. It is
  ## passed through when there is one only so that the file is self describing;
  ## without one GDAL says it is substituting an undefined Cartesian system,
  ## which is true and not worth printing.
  pts <- sf::st_as_sf(data.frame(x = s$xy[, 1L], y = s$xy[, 2L],
                                 value = s$value),
                      coords = c("x", "y"),
                      crs = if (is.null(s$grid$crs)) NA else s$grid$crs)
  if (is.null(s$grid$crs)) {
    suppressMessages(sf::st_write(pts, src, quiet = TRUE))
  } else {
    sf::st_write(pts, src, quiet = TRUE)
  }

  dm <- s$grid$dimension
  ex <- s$grid$extent
  sf::gdal_utils("grid", src, dst, options = as.character(
    c("-a", algorithm,
      "-txe", ex[1L], ex[2L], "-tye", ex[3L], ex[4L],
      "-outsize", dm[1L], dm[2L],
      "-zfield", "value", "-of", "GTiff", "-ot", "Float64", ...)))

  ds <- methods::new(gdalraster::GDALRaster, dst)
  on.exit(ds$close(), add = TRUE, after = FALSE)
  s$grid$values <- ds$read(band = 1L, xoff = 0L, yoff = 0L,
                           xsize = dm[1L], ysize = dm[2L],
                           out_xsize = dm[1L], out_ysize = dm[2L])
  s$grid
}
