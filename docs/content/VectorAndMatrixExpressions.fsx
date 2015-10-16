(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
Vector and Matrix Expressions
=============================
Vector and matrix expressions are expressions which are composed of elementwise functions and operators, e.g.:
*)
#r "FCore.dll"
open FCore
open FCore.Math
open FCore.ExplicitConversion
open System

let x = new Matrix(1000, 1000, 1.0) 
let y = new Matrix(1000, 1000, 2.0)
let z = 2.2 * x + 3.3 * sin(y)
(** 
When `z` is evaluated, a few temporary matrices are created in memory, e.g. `2.2 * x`.
These temporary matrices will use a lot of memory and will increase .NET Garbage Collection time.
In order to avoid this negative impact on performance we can use lazy evaluation of expressions:
*)
let zExpr = 2.2 * x.AsExpr + 3.3 * sin(y.AsExpr)
(** 
`zExpr` is of type `MatrixExpr`. It means the expression has not been evaluated yet and we need to apply a function `eval` to get the result:
*)
let z2 = zExpr |> eval
(** 
FCore will evaluate the expression in small slices, by default 100 000 elements at a time, so that only very small memory buffers are allocated.
The evaluation will not create any additional .NET objects so that Garbage Collection is minimized.
The eval slice should be small enough to fit into CPU cache for maximum performance. You can change it to any value:
*)
Vector.EvalSliceLength <- 150000 // MatrixExpr and VectorExpr evaluation uses Vector slicing
BoolVector.EvalSliceLength < 75000 // BoolMatrixExpr and BoolVectorExpr evaluation uses BoolVector slicing
(** 
You can create expressions for each of the numeric types: `BoolVectorExpr`, `VectorExpr`, `BoolMatrixExpr` and `MatrixExpr`.
All elementwise functions and operators are overloaded and defined for expressions.
Expressions can also be evaluated in place:
*)
evalIn (2.2 * x.AsExpr + 3.3 * sin(y.AsExpr)) x // the result of expression evaluation will be stored in x
(** 
You can create more complex expressions with `iif` function:
*)
let z3 = iif (x.AsExpr .< 2.0) (x.AsExpr + 1.0) (sin(y.AsExpr)) |> eval
(** 
`iif` takes 3 arguments: conditional expression, expression when condition is true and expression when condition is false.
It has many overloads: the first argument must be `BoolVectorExpr` or `BoolMatrixExpr`.
If condition is `BoolVectorExpr` then the other 2 args must be both `bool`, `float`, `BoolVector` or `Vector`.
If condition is `BoolMatrixExpr` then the other 2 args must be both `bool`, `float`, `BoolMatrix` or `Matrix`, e.g.
*)
let z4 = iif (x.AsExpr .<= 2.0) x y |> eval
let z5 = iif (x.AsExpr .> y) 1.0 0.0 |> eval





