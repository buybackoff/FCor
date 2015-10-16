(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
Basic Stats
============
FCore provides functions for calculating basic sample statistics, e.g. `mean`, `var` or `corr`, as well as cumulative sum and product.
You can apply these functions to vectors or matrices.
  
Basic statistics
--------------------
The following functions calculate basic sample statistics of vectors and matrices:

- `min` calculates minimum of vector elements or matrix rows/columns
- `max` calculates maximum of vector elements or matrix rows/columns
- `sum` calculates sum of vector elements or matrix rows/columns
- `prod` calculates product of vector elements or matrix rows/columns
- `mean` calculates mean of vector elements or matrix rows/columns
- `var` calculates unbiased variance of vector elements or matrix rows/columns
- `skewness` calculates not corrected for bias skewness of vector elements or matrix rows/columns
- `kurtosis` calculates not corrected for bias kurtosis of vector elements or matrix rows/columns

When applied to a vector, those functions return a scalar value, e.g.:
*)
#r "FCore.dll"
open FCore
open FCore.BasicStats
open FCore.ExplicitConversion
open FCore.Random   
open System

let v1 : Vector= !![1.0;2.0;3.0]
let mu = mean v1
(** This gives a value for mu of: *)
(*** include-value: mu ***)
(**
When applied to a matrix, they return a vector, e.g.:
*)
let m1 : Matrix = !![[1.0;2.0]
                     [3.0;4.0]]
let rowSum = sum m1 RowAxis
let colProd = prod m1 ColumnAxis
(** This gives a value for rowSum of: *)
(*** include-value: rowSum ***)
(** This gives a value for colProd of: *)
(*** include-value: colProd ***)
(**
Cumulative sum and product
---------------------------
You can also calculate cumulative sum or product of vector or matrix elements.
*)
let v2 = cumsum v1
(** This gives a value for v2 of: *)
(*** include-value: v2 ***)
let m2 = cumprod m1 RowAxis
(** This gives a value for m2 of: *)
(*** include-value: m2 ***)
(**
Quantiles
---------
`quantile` function calculates quantiles for a range of probabilities:
*)
let rng = new MT19937Rng()
let p = !![0.25;0.5;0.75] // we want 25%, 50% and 75% quantiles
let v3 : Vector = rand rng 10000 // generate 10000 random numbers between [0,1]
let q1 = quantile v3 p
(** This gives a value for q1 of: *)
(*** include-value: q1 ***)
let m3 = rand rng 10000 3
let q2 = quantile m3 p ColumnAxis
(** This gives a value for q2 of: *)
(*** include-value: q2 ***)
(**
Correlation and covariance
---------
You can calculate correlation and covariance of a matrix [Nxp]. It is assumed that each matrix column is a vector of N observations.
The functions return a matrix [pxp]:
*)
let m4 = rand rng 10000 3
let corr' = corr m4
let cov' = cov m4
(** This gives a value for corr' of: *)
(*** include-value: corr' ***)
(** This gives a value for cov' of: *)
(*** include-value: cov' ***)




