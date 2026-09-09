# Cell size in each direction

Cell size in each direction

## Usage

``` r
grid_res(grid)
```

## Arguments

- grid:

  a `guerrilla_grid`

## Value

Two numbers, the x and y size of one cell.

## Examples

``` r
grid_res(grid_spec(dimension = c(4, 2), extent = c(0, 8, 0, 4)))
#> [1] 2 2
```
