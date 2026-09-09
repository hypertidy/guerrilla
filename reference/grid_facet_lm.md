# Fit a plane within each Delaunay triangle

Triangulate the points, fit `value ~ x + y` by least squares to the
three corners of each triangle, and evaluate that fit at the cells
inside it.

## Usage

``` r
grid_facet_lm(x, value = NULL, grid = NULL, duplicates = mean, ...)
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

This is the long way round to
[`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md),
and the two agree to floating point. Three points determine a plane,
least squares through three points is that plane, and barycentric
interpolation evaluates that same plane – so the expensive per-triangle
[`lm()`](https://rdrr.io/r/stats/lm.html) and the one vectorised
`tsearch()` call compute the same surface. Worth seeing once.

It replaces the Delaunay half of
[`facets()`](https://hypertidy.github.io/guerrilla/reference/facets.md),
which did this through spatstat and returned a point pattern rather than
a grid.

The Voronoi half of
[`facets()`](https://hypertidy.github.io/guerrilla/reference/facets.md)
is not here, because it was not doing what it looked like it was doing.
A Voronoi tile contains exactly one point by construction, so
`lm(value ~ x + y)` on it fits an intercept and nothing else, and
predicts that one point's value across the whole tile. That is nearest
neighbour, computed the slowest way available.
[`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md)
is the same answer under its right name.

## See also

[`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md)
for the fast form of this,
[`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md)
for what the Voronoi case really was.

## Examples

``` r
xy <- cbind(runif(40), runif(40))
v <- xy[, 1] + xy[, 2]
plot(grid_facet_lm(xy, v))
```
