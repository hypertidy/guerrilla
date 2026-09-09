# Coordinates and values, from whatever you have

Every interpolation function here takes the same thing: places, and a
value at each place. This is what turns the various ways of expressing
that into one matrix of coordinates and one vector of values.

## Usage

``` r
xyz_input(x, value = NULL)
```

## Arguments

- x:

  coordinates: a matrix, a data frame, or anything wk handles

- value:

  one value per coordinate, or `NULL` to use the z of `x`

## Value

A list of `xy` (a two column matrix), `value`, and `crs`.

## Details

A value can arrive two ways, and they are the same thing said
differently: as a separate `value` argument, or as the z of three
dimensional coordinates. Barycentric interpolation really is treating
the value as a height above the plane, so
[`wk::xyz()`](https://paleolimbot.github.io/wk/reference/xy.html) input
is not an abuse of notation here, it is the notation. For kriging or a
GAM it would be, which is why `value` exists too.

Objects from wk and sf go through
[`wk::wk_coords()`](https://paleolimbot.github.io/wk/reference/wk_vertices.html),
which also carries their coordinate reference system across. A plain
matrix or data frame is read by position: first column x, second y,
third z if present.
[`wk::as_xy()`](https://paleolimbot.github.io/wk/reference/xy.html) is
deliberately not used for those, because it refuses to guess at column
names, and `Lon`/`Lat` is exactly what real data is called.

## Examples

``` r
xyz_input(cbind(1:3, 4:6), c(10, 20, 30))
#> $xy
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
#> 
#> $value
#> [1] 10 20 30
#> 
#> $crs
#> NULL
#> 

## z is the value
xyz_input(cbind(1:3, 4:6, c(10, 20, 30)))
#> $xy
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
#> 
#> $value
#> [1] 10 20 30
#> 
#> $crs
#> NULL
#> 

## and the crs comes along
xyz_input(wk::xyz(1:3, 4:6, c(10, 20, 30), crs = "EPSG:4326"))$crs
#> [1] "EPSG:4326"
```
