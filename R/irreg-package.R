#' @keywords internal
"_PACKAGE"


#' Bathymetry and topography for the BROKE-West region
#'
#' A gridded elevation layer covering the Southern Ocean sector surveyed by the
#' BROKE-West voyage, used as a backdrop for the interpolation examples.
#'
#' Derived from ETOPO2, cropped to 15 to 95 degrees east and 75 to 55 degrees
#' south, aggregated by a factor of 2, smoothed with a Gaussian focal weight,
#' and rounded to whole metres. Values are elevation relative to sea level, so
#' the ocean is negative and the Antarctic continent is positive. Cells around
#' the edge are `NA` where the focal window ran off the grid.
#'
#' @format A `RasterLayer` of 300 rows by 1200 columns at 1/15 degree
#' resolution, in longitude/latitude on WGS84. Values are integer metres, from
#' -5574 to 3612, with 17280 of 360000 cells missing.
#' @source ETOPO2, via `data-raw/bathy.R`. See
#' `inst/examples/afternoon_exercise.Rmd` for how it was built.
#' @name bathy
NULL
