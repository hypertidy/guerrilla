# Collapse coordinates that repeat

Triangulation has nothing to say about two values at the same place, and
Qhull refuses outright. Real data does this constantly: a station
occupied twice, a transect that crosses its own track, coordinates
rounded to a printed precision.

## Usage

``` r
collapse_duplicates(
  xy,
  value,
  fun = mean,
  quiet = FALSE,
  fun_label = deparse(substitute(fun))
)
```

## Arguments

- xy:

  coordinates, two columns

- value:

  one value per row of `xy`

- fun:

  function used to combine the values at a repeated coordinate

- quiet:

  do not report how many coordinates were collapsed

- fun_label:

  name of `fun` to use in that report

## Value

A list of `xy` and `value`, with no repeated coordinate.

## Details

So decide explicitly. `collapse_duplicates()` reduces repeated
coordinates to one, combining their values with `fun`, and says how many
it collapsed.

## Examples

``` r
xy <- cbind(c(1, 2, 2, 3), c(1, 2, 2, 3))
collapse_duplicates(xy, c(10, 20, 30, 40))
#> 1 duplicated coordinate collapsed with mean()
#> $xy
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    2    2
#> [3,]    3    3
#> 
#> $value
#> [1] 10 25 40
#> 
```
