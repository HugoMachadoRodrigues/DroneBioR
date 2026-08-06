# Default number of ODM workers for this machine

One less than the usable physical cores, floored at 1 and capped at
`cap`. Leaving a core free keeps the machine usable while a
reconstruction runs, which matters because these runs last tens of
minutes. Physical cores rather than logical ones: OpenSfM and OpenMVS
workers are compute- and memory-bound, so hyperthreads buy little while
doubling the memory demand.

## Usage

``` r
default_max_concurrency(cap = 16L)
```

## Arguments

- cap:

  Upper bound on the result. Each worker costs on the order of 1-2 GB,
  so an unbounded count on a very large machine trades speed for memory
  pressure; on a 16 GB machine a smaller explicit `max_concurrency` is
  worth passing.

## Value

A positive integer.

## Details

The previous fixed default of 4 was written for a modest laptop and
quietly throttled bigger ones: on a 10-core M1 Max a run sat at ~320%
CPU and 7% of the memory Docker had been given, with feature matching –
the longest part of `opensfm` – using less than a third of the machine.

The count is the smaller of the host's cores and the CPUs Docker
reports, because ODM runs inside the container: sizing to a 10-core host
while Docker holds 4 would start more workers than there are cores, and
each worker holds imagery, so the run gets slower and likelier to be
OOM-killed.

Set `options(dronebior.max_concurrency = n)` to pin a value outright.

## Examples

``` r
default_max_concurrency()
#> [1] 3
```
