# Interpolate with GDAL's own gridder

Hand the points to GDAL and let `gdal_grid` do the interpolation,
returning the result as a grid like everything else here.

## Usage

``` r
grid_gdal(
  x,
  value = NULL,
  grid = NULL,
  algorithm = "linear",
  nodata = NaN,
  ...
)
```

## Arguments

- x:

  coordinates, or coordinates carrying their value as z; see
  [`xyz_input()`](https://hypertidy.github.io/guerrilla/reference/xyz_input.md)

- value:

  one value per coordinate, or `NULL` to use the z of `x`

- grid:

  a
  [`grid_spec()`](https://hypertidy.github.io/guerrilla/reference/grid_spec.md)
  to interpolate onto, or `NULL` for a default one

- algorithm:

  a `gdal_grid` algorithm string, passed to `-a`

- nodata:

  value GDAL should write for empty cells, read back as `NA`; ignored if
  `algorithm` already sets one

- ...:

  further arguments appended to the `gdal_grid` command line

## Value

A `guerrilla_grid` with values.

## Details

Four of the methods in this package have a `gdal_grid` twin, and running
them side by side is the best available check that any of them is right.
They are independent implementations in different languages by different
people, so agreement is evidence and disagreement is a bug in one of
them:

|  |  |
|----|----|
| `"linear:radius=0.0"` | [`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md) |
| `"nearest"` | [`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md) |
| `"invdist:power=2.0"` | [`grid_idw()`](https://hypertidy.github.io/guerrilla/reference/grid_idw.md) |
| `"average:radius1=,radius2="` | [`grid_bin()`](https://hypertidy.github.io/guerrilla/reference/grid_bin.md), with a search radius |

## Two GDAL defaults worth knowing

`algorithm = "linear"` on its own does not stop at the convex hull.
GDAL's default is `radius=-1`, an infinite search, so a cell in no
triangle takes the value of the nearest point instead of being left out.
That is extrapolation, it is silent, and it is why `"linear:radius=0.0"`
rather than `"linear"` is the one that matches
[`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md).

Second, `gdal_grid` fills cells it could not estimate with 0 and does
not record that 0 as the band's no data value, so nothing downstream can
tell it apart from a real measurement – and 0 is a real temperature, a
real elevation and a real anomaly. This function therefore appends
`nodata=nan` unless the algorithm string already sets one. That both
fills the empty cells with `NaN` and tags the band, and gdalraster turns
the tagged value into `NA` on the way back, so an explicit
`nodata=-9999` arrives here as `NA` just the same.

## Why sf

`gdal_grid` is reachable from R only through
[`sf::gdal_utils()`](https://r-spatial.github.io/sf/reference/gdal_utils.html).
gdalraster wraps `warp`, `translate` and `rasterize` but not `GDALGrid`,
and the unified `gdal` command line added in GDAL 3.11 has no grid
subcommand either, so there is nothing for it to wrap yet.

## See also

[`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md),
[`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md),
[`grid_idw()`](https://hypertidy.github.io/guerrilla/reference/grid_idw.md)

## Examples

``` r
xy <- cbind(runif(50), runif(50))
if (requireNamespace("sf", quietly = TRUE) &&
    requireNamespace("gdalraster", quietly = TRUE)) {
  g <- grid_spec(xy, dimension = c(40, 40))
  gdal <- grid_gdal(xy, xy[, 1], g, algorithm = "linear:radius=0.0")
  ours <- grid_barycentric(xy, xy[, 1], g)
  max(abs(gdal$values - ours$values), na.rm = TRUE)
}
#> [1] 8.881784e-16
```
