(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCor"
(**
Vector and Matrix Functions
============
FCor provides math functions which can be applied elementwise to vectors and matrices, e.g. `sin`.
In addition, there are function for manipulating vectors and matrices, e.g. `diag` and `triL`. 
  
Elementwise math functions
--------------------
The following math functions can be applied to scalars, vectors and matrices:

- `abs` calculates absolute value
- `sqrt` calculates square root
- `exp` calculates exponential
- `expm1` calculates exp(x)-1 accurately for small values
- `log` calculates natural logarithm
- `log1p` calculates log(1+x) accurately for small values
- `log10` calculates base 10 logarithm
- `cos` calculates cosine
- `sin` calculates sine
- `tan` calculates tangent
- `acos` calculates inverse cosine
- `asin` calculates inverse sine
- `atan` calculates inverse tangent
- `cosh` calculates hyperbolic cosine
- `sinh` calculates hyperbolic sine
- `tanh` calculates hyperbolic tangent
- `acosh` calculates inverse hyperbolic cosine
- `asinh` calculates inverse hyperbolic sine
- `atanh` calculates inverse hyperbolic tangent
- `erf` calculates error function
- `erfc` calculates complementary error function
- `erfinv` calculates inverse error function
- `erfcinv` calculates inverse complementary error function
- `normcdf` calculates standard normal cumulative distribution function
- `norminv` calculates inverse standard normal cumulative distribution function
- `floor` rounds to integer value towards minus infinity
- `ceil` rounds to integer value towards plus infinity
- `round` rounds to nearest integer
- `trunc` truncates fractional part

*)
#r "FCor.dll"
open FCor
open FCor.Math
open FCor.ExplicitConversion
open System

let v1 : Vector= !![1.0;2.0;3.0]
let m1 : Matrix = !![[1.0;2.0;3.0]
                     [4.0;5.0;6.0]]
let a = exp 2.0
let v2 = exp v1
let m2 = exp m1
(** This gives a value for a of: *)
(*** include-value: a ***)
(** This gives a value for v2 of: *)
(*** include-value: v2 ***)
(** This gives a value for m2 of: *)
(*** include-value: m2 ***)
(**
Manipulating vectors and matrices
---------------------------------
You can extract lower or upper triangular matrix:
*)
let m3 : Matrix = !![[1.0;2.0;3.0]
                     [4.0;5.0;6.0]
                     [7.0;8.0;9.0]
                     [10.0;11.0;12.0]]
let m4 = triU m3 0 // offset = 0 means main diagonal
let m5 = triL m3 1L// offset = 0L mean 1 above main diagonal, note offset can be int64
(** This gives a value for m4 of: *)
(*** include-value: m4 ***)
(** This gives a value for m5 of: *)
(*** include-value: m5 ***)
(**
`diag` function can extract main or offset diagonal from a matrix:
*)
let v3 = diag m3 -1 // extract diagonal below main diagonal as vector
(** This gives a value for v3 of: *)
(*** include-value: v3 ***)
(**
`diag` can also create a matrix with given diagonal from a vector:
*)
let v4 : Vector = !![1.0;2.0;3.0]
let m6 = diag v4 1 //diagonal above main diagonal will be equal to given vector, all other elements are zero
(** This gives a value for m6 of: *)
(*** include-value: m6 ***)



