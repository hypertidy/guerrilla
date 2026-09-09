# Kernel smoothing on a grid

Bin the points to a grid, then smooth that grid with a Gaussian kernel,
using
[`fields::image.smooth()`](https://rdrr.io/pkg/fields/man/image.smooth.html).

## Usage

``` r
grid_smooth(x, value = NULL, grid = NULL, theta = NULL, ...)
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

- theta:

  kernel bandwidth, in the units of the coordinates

- ...:

  passed to
  [`fields::image.smooth()`](https://rdrr.io/pkg/fields/man/image.smooth.html)

## Value

A `guerrilla_grid` with values.

## Details

This is the two step method: rasterize first
([`grid_bin()`](https://hypertidy.github.io/guerrilla/reference/grid_bin.md)),
estimate second. Everything else here works from the points directly, so
this one is worth having as the contrast – the binning throws away where
inside its cell each point was, and no amount of smoothing afterwards
gets that back.

`theta` is the kernel bandwidth in coordinate units, and it does all the
work. There is no standard error, because there is no model.

[`fields::image.smooth()`](https://rdrr.io/pkg/fields/man/image.smooth.html)
wants a square grid, so this uses one of `max(dimension)` cells a side
and returns it; the result covers the same extent but need not have the
dimension you asked for.

## Examples

``` r
xy <- cbind(runif(200), runif(200))
if (requireNamespace("fields", quietly = TRUE)) {
  plot(grid_smooth(xy, xy[, 1] + xy[, 2], theta = 0.1))
}
```
