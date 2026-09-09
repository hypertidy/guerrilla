# guerrilla dev

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
