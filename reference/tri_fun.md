# Interpolation to a regular grid via triangulation

Superseded by
[`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md),
which does the same thing under a name that says what it is.

## Usage

``` r
tri_fun(xy, value, grid = NULL, duplicates = mean, ...)
```

## Arguments

- xy:

  coordinates

- value:

  value to interpolate

- grid:

  grid to use

- duplicates:

  passed to
  [`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md)

- ...:

  passed to
  [`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md)

## Value

A `guerrilla_grid` with values.

## Examples

``` r
xy <- cbind(runif(50), runif(50))
grid_barycentric(xy, xy[, 1])  ## use this instead
#> <guerrilla grid>
#> dimension : 60, 50  (ncol, nrow) = 3000 cells
#> extent    : 0.03968992, 0.92577588, 0.04549673, 0.95116504  (xmin, xmax, ymin, ymax)
#> resolution: 0.01476810, 0.01811337
#> crs       : <none>
#> values    : 2745 of 3000 cells, 0.04707397 to 0.9183918
```
