# Interpolate by fitting a plane within each tessellation facet

Superseded. Use
[`grid_facet_lm()`](https://hypertidy.github.io/guerrilla/reference/grid_facet_lm.md)
for the Delaunay case and
[`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md)
for the Dirichlet one, both of which take the same arguments as
everything else here and return a grid rather than a point pattern.

## Usage

``` r
facets(
  X,
  nx,
  ny,
  x = NULL,
  y = NULL,
  na.v = 0,
  method = c("dirichlet", "delaunay")
)
```

## Arguments

- X:

  spatstat object

- nx:

  number of x coords

- ny:

  number of y coords

- x:

  option input x values

- y:

  optional input y values

- na.v:

  na value

- method:

  dirichlet or delaunay

## Value

ppp object

## Details

Tessellate a marked point pattern into Dirichlet (Voronoi) cells or
Delaunay triangles, fit a linear trend in x and y to the marks falling
within each facet, and predict that trend at a set of grid locations.

Worth knowing what the two methods were: a Dirichlet tile contains
exactly one point, so `method = "dirichlet"` fits an intercept and
nothing else, and predicts that one point's value across its whole tile.
It is nearest neighbour, by way of a linear model per tile in an R loop.
[`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md)
is the same numbers, and says so.
