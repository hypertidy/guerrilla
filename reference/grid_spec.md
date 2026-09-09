# A grid, as a plain list

Describe a regular grid: how many cells across and down, what region of
the plane it covers, and optionally what coordinate system that plane is
in.

## Usage

``` r
grid_spec(
  x = NULL,
  dimension = c(60L, 50L),
  extent = NULL,
  crs = NULL,
  pad = 0
)
```

## Arguments

- x:

  coordinates to cover: anything with two or more columns, such as a
  matrix or data frame. Ignored when `extent` is given.

- dimension:

  number of columns and rows, in that order

- extent:

  outer edges `c(xmin, xmax, ymin, ymax)`; taken from `x` if not given

- crs:

  coordinate reference system, or `NULL` (the default) for none

- pad:

  fraction of each axis range to add on every side, so the outermost
  input coordinates do not sit exactly on the boundary. `0` by default.

## Value

A `guerrilla_grid`, which is a list of `dimension`, `extent`, `crs` and
`values`.

## Details

The value returned is a list. That is deliberate. Every raster in every
package is some version of these few numbers, and a reader who can print
the whole thing has actually been told what a raster is:

- `dimension`, the number of columns and rows, in that order;

- `extent`, the outer edges as `xmin, xmax, ymin, ymax`;

- `crs`, a coordinate reference system, or `NULL` for none;

- `values`, one number per cell, or `NULL` for a grid that is only a
  target.

`dimension` and `extent` are exactly the first two arguments of the
vaster functions, so `grid$dimension` and `grid$extent` can be handed
straight to
[`vaster::xy_from_cell()`](https://hypertidy.github.io/vaster/reference/cells.html)
and friends. Cells are numbered left to right along the top row first,
the same order raster and GDAL use.

Nothing here assumes the plane is the Earth. `crs` stays `NULL` unless
you say otherwise.

## Examples

``` r
xy <- cbind(c(0, 4, 10), c(0, 2, 5))
grid_spec(xy)
#> <guerrilla grid>
#> dimension : 60, 50  (ncol, nrow) = 3000 cells
#> extent    : 0, 10, 0, 5  (xmin, xmax, ymin, ymax)
#> resolution: 0.1666667, 0.1000000
#> crs       : <none>
#> values    : <none>

## it really is just a list
str(unclass(grid_spec(xy)))
#> List of 4
#>  $ dimension: int [1:2] 60 50
#>  $ extent   : num [1:4] 0 10 0 5
#>  $ crs      : NULL
#>  $ values   : NULL

## give the points some room
grid_spec(xy, pad = 0.05)$extent
#> [1] -0.50 10.50 -0.25  5.25

## or skip the points entirely
grid_spec(dimension = c(4, 3), extent = c(0, 8, 0, 6))
#> <guerrilla grid>
#> dimension : 4, 3  (ncol, nrow) = 12 cells
#> extent    : 0, 8, 0, 6  (xmin, xmax, ymin, ymax)
#> resolution: 2, 2
#> crs       : <none>
#> values    : <none>
```
