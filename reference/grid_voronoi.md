# Nearest neighbour interpolation, by Voronoi tessellation

Give every cell the value of the closest input point.

## Usage

``` r
grid_voronoi(x, value = NULL, grid = NULL, duplicates = mean, ...)
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

- duplicates:

  function used to combine values at repeated coordinates, or `NULL` to
  leave them alone

- ...:

  ignored

## Value

A `guerrilla_grid` with values.

## Details

The tessellation is the picture of that rule. A Voronoi tile is exactly
the region closer to its own point than to any other, so assigning each
cell to the tile it falls in is nearest neighbour, drawn rather than
computed.

Unlike
[`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md),
this fills the whole grid. Voronoi tiles cover the plane, so there is no
convex hull to fall outside of – which is convenient and is also the
method's main way of lying to you, since a cell far beyond the data gets
a confident answer from one distant point.

## Examples

``` r
xy <- cbind(runif(30), runif(30))
plot(grid_voronoi(xy, xy[, 1]))
points(xy, pch = 16, cex = 0.5)
```
