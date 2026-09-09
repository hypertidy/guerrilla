# Package index

## The grid

A grid is a list of four things: dimension, extent, crs, values. These
make one, ask it questions, and hand it to another package.

- [`grid_spec()`](https://hypertidy.github.io/guerrilla/reference/grid_spec.md)
  : A grid, as a plain list
- [`grid_xy()`](https://hypertidy.github.io/guerrilla/reference/grid_xy.md)
  : Coordinates of the cell centres
- [`grid_ncell()`](https://hypertidy.github.io/guerrilla/reference/grid_ncell.md)
  : Number of cells in a grid
- [`grid_res()`](https://hypertidy.github.io/guerrilla/reference/grid_res.md)
  : Cell size in each direction
- [`is_grid()`](https://hypertidy.github.io/guerrilla/reference/is_grid.md)
  : Is this a guerrilla grid?
- [`as_grid()`](https://hypertidy.github.io/guerrilla/reference/as_grid.md)
  : Read another package's raster as a grid
- [`as_raster()`](https://hypertidy.github.io/guerrilla/reference/converters.md)
  [`as_terra()`](https://hypertidy.github.io/guerrilla/reference/converters.md)
  [`as_gdalraster()`](https://hypertidy.github.io/guerrilla/reference/converters.md)
  : Hand a grid to another package
- [`plot(`*`<guerrilla_grid>`*`)`](https://hypertidy.github.io/guerrilla/reference/plot.guerrilla_grid.md)
  : Plot a grid
- [`as.matrix(`*`<guerrilla_grid>`*`)`](https://hypertidy.github.io/guerrilla/reference/as.matrix.guerrilla_grid.md)
  : Turn a grid into a matrix
- [`as.data.frame(`*`<guerrilla_grid>`*`)`](https://hypertidy.github.io/guerrilla/reference/as.data.frame.guerrilla_grid.md)
  : Turn a grid into a data frame

## Interpolation

Every one of these takes coordinates, values and a grid, and returns a
grid. They differ in what they assume, and the documentation says what.

- [`grid_bin()`](https://hypertidy.github.io/guerrilla/reference/grid_bin.md)
  : Bin values into grid cells
- [`grid_voronoi()`](https://hypertidy.github.io/guerrilla/reference/grid_voronoi.md)
  : Nearest neighbour interpolation, by Voronoi tessellation
- [`grid_barycentric()`](https://hypertidy.github.io/guerrilla/reference/grid_barycentric.md)
  : Interpolate to a grid across a triangulation
- [`grid_idw()`](https://hypertidy.github.io/guerrilla/reference/grid_idw.md)
  : Inverse distance weighted interpolation
- [`grid_tps()`](https://hypertidy.github.io/guerrilla/reference/grid_tps.md)
  : Thin plate spline interpolation
- [`grid_kriging()`](https://hypertidy.github.io/guerrilla/reference/grid_kriging.md)
  : Kriging
- [`grid_gam()`](https://hypertidy.github.io/guerrilla/reference/grid_gam.md)
  : Generalized additive model on the coordinates
- [`grid_smooth()`](https://hypertidy.github.io/guerrilla/reference/grid_smooth.md)
  : Kernel smoothing on a grid
- [`grid_gdal()`](https://hypertidy.github.io/guerrilla/reference/grid_gdal.md)
  : Interpolate with GDAL's own gridder
- [`mesh_raster()`](https://hypertidy.github.io/guerrilla/reference/mesh_raster.md)
  : Mesh raster

## The arithmetic, in the open

The parts other packages keep in C. These exist to be read, and to check
that the fast paths compute what they claim to.

- [`bary_weights()`](https://hypertidy.github.io/guerrilla/reference/bary_weights.md)
  : Barycentric coordinates of points within a triangle
- [`find_triangle()`](https://hypertidy.github.io/guerrilla/reference/find_triangle.md)
  : Which triangle is each point in?
- [`grid_facet_lm()`](https://hypertidy.github.io/guerrilla/reference/grid_facet_lm.md)
  : Fit a plane within each Delaunay triangle

## Input

- [`xyz_input()`](https://hypertidy.github.io/guerrilla/reference/xyz_input.md)
  : Coordinates and values, from whatever you have
- [`collapse_duplicates()`](https://hypertidy.github.io/guerrilla/reference/collapse_duplicates.md)
  : Collapse coordinates that repeat

## Data

- [`bathy`](https://hypertidy.github.io/guerrilla/reference/bathy.md) :
  Bathymetry and topography for the BROKE-West region

## The package

- [`guerrilla`](https://hypertidy.github.io/guerrilla/reference/guerrilla-package.md)
  [`guerrilla-package`](https://hypertidy.github.io/guerrilla/reference/guerrilla-package.md)
  : guerrilla: Illustrate Various Methods of Interpolation for Irregular
  Data

## Superseded

Kept so old code runs. Each one’s help page says what to use instead and
why.

- [`tri_fun()`](https://hypertidy.github.io/guerrilla/reference/tri_fun.md)
  : Interpolation to a regular grid via triangulation
- [`defaultgrid()`](https://hypertidy.github.io/guerrilla/reference/defaultgrid.md)
  : Create a target grid for interpolation
- [`facets()`](https://hypertidy.github.io/guerrilla/reference/facets.md)
  : Interpolate by fitting a plane within each tessellation facet
