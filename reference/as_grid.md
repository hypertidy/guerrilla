# Read another package's raster as a grid

Take the dimension, extent and coordinate system off a `RasterLayer` or
a terra `SpatRaster` and return them as a
[`grid_spec()`](https://hypertidy.github.io/guerrilla/reference/grid_spec.md).
Values are not carried over; this is for describing a target to
interpolate onto.

## Usage

``` r
as_grid(x)
```

## Arguments

- x:

  a `guerrilla_grid`, a `RasterLayer`, or a `SpatRaster`

## Value

A `guerrilla_grid`.

## Details

It exists so that a grid you already have still works as a target, and
so that the claim these classes are all the same few numbers can be
checked rather than taken on trust.

## Examples

``` r
if (requireNamespace("raster", quietly = TRUE)) {
  as_grid(raster::raster(raster::extent(0, 8, 0, 6), ncols = 4, nrows = 3))
}
#> <guerrilla grid>
#> dimension : 4, 3  (ncol, nrow) = 12 cells
#> extent    : 0, 8, 0, 6  (xmin, xmax, ymin, ymax)
#> resolution: 2, 2
#> crs       : <none>
#> values    : <none>
```
