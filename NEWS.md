# guerrilla 0.3.0.9000

## The grid is a list now

Everything that returned a `RasterLayer` returns a `guerrilla_grid`, which is a
list of four things and nothing else: `dimension` (ncol, nrow), `extent`
(xmin, xmax, ymin, ymax), `crs`, and `values`. `str()` on one is the complete
explanation of what a raster is, which is the point.

* New `grid_spec()` builds one, with optional `pad` and no coordinate system
unless you ask for one. It supersedes `defaultgrid()`, which is deprecated.

* New `grid_xy()`, `grid_ncell()`, `grid_res()`, `is_grid()`, and `print()`,
`plot()`, `as.matrix()`, `as.data.frame()` and `dim()` methods. `plot()` draws
the grid with `graphics::image()`, no raster class involved.

* New `as_raster()`, `as_terra()`, `as_gdalraster()` to hand a grid to another
package, and `as_grid()` to read one back. `tri_fun()` and `mesh_raster()`
still accept a `RasterLayer` or `SpatRaster` as the target.

* Cell arithmetic now goes through **vaster**, which does it in base R.
`raster` and `sp` leave Imports; the only hard dependencies left are
`geometry` and `vaster`. `library(guerrilla)` no longer loads raster at all.

* `data(bathy)` was a serialized `RasterLayer`, so the dataset alone made
raster a runtime requirement whatever the code did. It is a grid now, and
loads with no other package present.

The numbers did not change. `tri_fun()` and `mesh_raster()` produce values
identical to 0.2.0 on volcano, on the zooplankton transect, and on `quakes`.

## Repeated coordinates

A triangulation cannot hold two values at one place, and Qhull's answer was to
drop points and warn about it. `tri_fun()` and `mesh_raster()` now combine them
first, with `duplicates = mean` by default, and say how many they combined.
`duplicates = NULL` restores the old behaviour exactly. New
`collapse_duplicates()` does it on its own.

Coordinates that are all on one line make Qhull return zero triangles and only
warn; that now raises an error that says what happened.

## Vignette

Rewritten onto the new grid, converting to a raster only where a third-party
function needs one -- which makes the boundary between this package and the
statistical engines visible, rather than incidental.

# guerrilla dev

* The vignette builds again. Two things were stopping it:

  - `as(tess, "SpatialPolygons")` was a coercion registered by maptools, so
  both tessellation plots died once maptools went away. They now use
  spatstat's own `plot.tess(do.col = TRUE)`, which needs no sp at all and
  gets a colour ribbon for free.

  - `interp::interp()` returns a grid of all `NA`, without complaint, when
  `yo` is descending -- and `raster::yFromRow()` is top-down. akima tolerated
  it. The vignette's `akifun()` now sorts the axis. The fixed version agrees
  with `tri_fun()` to 9e-16, which it should: both are linear interpolation
  over a Delaunay triangulation.

* `defaultgrid()` no longer asserts a longitude/latitude coordinate reference
system by default. Nothing in this package requires geographic input, so the
default was wrong for most uses -- including this package's own `tri_fun()`
example, which interpolated `volcano` and returned a grid labelled WGS84
degrees. Pass `prj` explicitly when the coordinates really are in a known
system.

* Removed `tri_pip()`, which was superseded by `geometry::tsearch()` and had
been unused since 2019. With it goes the dependency on sp.

* `geometry` moved from Suggests to Imports: `tri_fun()` and `mesh_raster()`
cannot work without it.

* `akima` replaced by `interp` in Suggests and in the vignette. akima's licence
is non-commercial; interp is GPL and covers the same cases.

* The `bathy` documentation described a polygon layer. It is a raster.

* Both GitHub Actions workflows rebuilt from the current r-lib/actions
templates; the old ones pinned actions and runner images that no longer exist.

* `inst/extdata/datamess.mat` removed (referenced nowhere), and the SAZ-Sense
transect compressed, taking installed data from 1.2 MB to 103 kB.

* Remove maptools

# guerilla 0.2.0

* Tiny release to bump the universe. 

* New function `mesh_raster()` a generalization of `tri_fun()` now
also works with 'mesh3d' input. 


# guerrilla 0.1.0

* Removed Suggests for rgdal in favour of reproj package. 

* Yuge speed up to `tri_fun` by using `geometry::tsearch`.
