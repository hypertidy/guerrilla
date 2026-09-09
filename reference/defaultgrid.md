# Create a target grid for interpolation

Superseded by
[`grid_spec()`](https://hypertidy.github.io/guerrilla/reference/grid_spec.md),
which returns a plain list rather than a `RasterLayer`, and which takes
its size as `dimension = c(ncol, nrow)`.

## Usage

``` r
defaultgrid(xy, ncols = 60, nrows = 50, prj = NA)
```

## Arguments

- xy:

  coordinates

- ncols:

  number of columns

- nrows:

  number of rows

- prj:

  projection metadata, `NA` (the default) for none

## Value

A `guerrilla_grid`, from
[`grid_spec()`](https://hypertidy.github.io/guerrilla/reference/grid_spec.md).

## Examples

``` r
xy <- cbind(c(0, 4, 10), c(0, 2, 5))
grid_spec(xy)  ## use this instead
#> <guerrilla grid>
#> dimension : 60, 50  (ncol, nrow) = 3000 cells
#> extent    : 0, 10, 0, 5  (xmin, xmax, ymin, ymax)
#> resolution: 0.1666667, 0.1000000
#> crs       : <none>
#> values    : <none>
```
