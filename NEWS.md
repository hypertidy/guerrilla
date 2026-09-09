# guerrilla 0.3.0.9000

## The methods are functions now, not closures in a vignette

Six of the methods the vignette demonstrated existed only as closures inside
it, each one wrapping its engine in `raster::interpolate()` and a bit of
coercion. They are exported functions with the same signature as everything
else: coordinates, values, a grid.

* New `grid_bin()`, `grid_tps()`, `grid_idw()`, `grid_kriging()`, `grid_gam()`
and `grid_smooth()`. Each takes `(x, value, grid)` and returns a grid, and each
one is guarded on the Suggests it needs with an error that names the package.

* `grid_tps()`, `grid_kriging()` and `grid_gam()` take
`statistic = "se"` and return the standard error surface on the same grid. This
is the thing most of these methods can say and none of them were saying. The
error surface next to the prediction is the whole argument for preferring a
method that fits a model over a method that applies a rule, and it looks
strikingly like `grid_bin(fun = length)`.

* `grid_idw()` has no `statistic`, on purpose. Inverse distance weighting is a
rule rather than a model, so there is nothing to be uncertain with, and `idp`
is chosen rather than estimated. That is the point of having it next to
`grid_kriging()`.

* `grid_kriging()` stops when the variogram fit comes back with a negative
range, which means the values have no spatial structure at these distances. It
is the only method here that can refuse; every other one will interpolate pure
noise and hand you a picture of it.

* `grid_gam()` takes a `formula`, defaulting to `value ~ s(x, y)` -- one
isotropic 2-D smooth, which is a thin plate regression spline, which is
`grid_tps()` with fewer basis functions. The vignette shows `value ~ s(x) +
s(y)` beside it, because additive in x and y cannot put a feature in one place.

* `grid_bin()` is the honest baseline and answers the question the others
answer silently: `fun` decides what happens when two points land in one cell.
`fun = length` gives the count grid, which is the most useful picture in the
vignette. It is `vaster::cell_from_xy()` and `tapply()`, and no more.

## No sp, and no raster in the middle of anything

**gstat** takes plain data frames with the coordinates given as a formula
(`locations = ~ x + y`), and **fields** and **mgcv** predict at a matrix of
coordinates. So none of these methods needs a spatial class: the statistical
engines never wanted one, they wanted coordinates and a `predict` method. The
conversion functions are still there for handing a result to another package,
but nothing in this package's own path goes through them any more.

`sp` leaves Suggests, along with `stars`, `dplyr` and `viridis`, which nothing
had used for some time.

## Vignette

Rewritten again, onto the exported functions. The closures are gone, each
section is now a call and an explanation of what the call assumed, and it ends
with all eight surfaces on one page -- which mostly shows that they agree where
there is data and disagree where there is not.

Removed about 130 lines of commented-out code at the end, which used sp,
maptools and spatstat interfaces that no longer exist.

## Interpolation has a name, and the weights are visible

`tri_fun()` said what it was implemented with. The methods are now named for
what they compute, and the arithmetic each one rests on is written out in R
next to the fast path, so the vignette can show it rather than assert it.

* New `grid_barycentric()` replaces `tri_fun()`, which is deprecated and still
works. Same numbers, plus a `duplicates` argument and an `engine` argument.

* New `bary_weights()` computes barycentric weights for a triangle in six lines
of arithmetic: the same three numbers are the point-in-triangle test and the
interpolation rule, which is the whole idea. New `find_triangle()` locates
points in a triangulation with **geos**, using an STRtree for candidates and
`bary_weights()` to decide. `grid_barycentric(engine = "R")` runs the whole
interpolation that way and agrees with `geometry::tsearch()` to 4e-16 on the
package's own example data.

* New `grid_voronoi()` fills a grid from the Voronoi tessellation, which is
nearest neighbour -- a tile is everywhere closer to one data point than to any
other. Built on `geos::geos_voronoi_polygons()`.

* New `grid_facet_lm()` fits `value ~ x + y` inside each Delaunay triangle.
Three points determine a plane, so this reproduces `grid_barycentric()`; it
exists because seeing that happen is the point.

  `facets(method = "dirichlet")` was not doing what it looked like. A Voronoi
  tile holds exactly one point, so the per-tile model had one observation and
  three parameters, fitted an intercept, and predicted that value across the
  tile. It was nearest neighbour computed the expensive way, and it is bitwise
  identical to `grid_voronoi()`. `facets()` is kept, superseded and documented
  as such.

* `mesh_raster()` on a `mesh3d` was calling a helper that did not exist, so
that path was broken. It now runs the second half of `grid_barycentric()`
directly: the triangles are already there, so there is nothing to triangulate.
There is a test for it that builds a `mesh3d` by hand, rather than needing Rvcg
installed to find out.

## Input is coordinates, not a class

* New `xyz_input()` reads points from anything **wk** understands -- `wk::xy()`,
`wk::xyz()`, sf and sfc columns, a matrix, a data frame -- and returns
coordinates, values and a coordinate reference system. Where a `z` is present it
is the value being interpolated, so `wk::xyz(x, y, z)` is a complete argument to
`grid_barycentric()` on its own.

* A coordinate reference system carried in on the input is carried out on the
grid. Nothing in the interpolation looks at it: the arithmetic is planar, and it
is the caller's business whether that is reasonable for their coordinates.

* **wk** and **geos** join Imports.

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

## Getting the package to build again

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

# guerrilla 0.2.0

* Tiny release to bump the universe. 

* New function `mesh_raster()` a generalization of `tri_fun()` now
also works with 'mesh3d' input. 


# guerrilla 0.1.0

* Removed Suggests for rgdal in favour of reproj package. 

* Yuge speed up to `tri_fun` by using `geometry::tsearch`.
