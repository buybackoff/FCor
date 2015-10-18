(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCor"


(**
FCor Numerical Library
======================

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The FCor library can be <a href="https://nuget.org/packages/FCor">installed from NuGet</a>:
      <pre>PM> Install-Package FCor</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Overview
--------

FCor is a high performance .NET numerical library with an F# API. It contains F# types and functions which will allow you to create and manipulate bool and float vectors and 2D dense matrices as well as generate random numbers and calculate basic stats. 
Main features:

- strong and static typing: use `BoolVector`, `Vector`, `BoolMatrix` and `Matrix` types in overloaded functions and operators
- FCor uses unmanaged memory so you can create vectors and matrices of virtually any size (64 bit memory required)
- most functions use Math Kernel Library (MKL) or C implementation for maximum performance on Intel
- elementwise operators: `.*`, `./`, `.<` etc
- access to all high quality random number generators from MKL
- access to matrix factorizations and solvers from MKL 
- overloaded functions, e.g. `rand` can return a vector or a matrix, `mean` can take a vector or matrix and axis etc.
- evaluate vector and matrix expressions without creating temporary objects, e.g. `(a * X + b * Y) |> eval` 
- vector and matrix slicing and indexing


Example
-------

This code example demonstrates using various functions defined in this library.

*)

#r "FCor.dll"
open FCor
open FCor.Math
open FCor.Random
open FCor.LinearAlgebra
open FCor.BasicStats

let rng = new MT19937Rng() // create a Mersenne Twister random number generator
let vector1 : Vector = rand rng 5 // create a random vector with 5 elements
let boolVector = vector1 .< 0.5 // compare elementwise
let matrix1 = rand rng 5 5 // create a random matrix 5x5
let vector2 = exp(vector1) + 2.0 // apply exp to a vector
let matrix2 = exp(matrix1) + 2.0 // apply exp to a matrix
let l, u, p = lu matrix1 // LU factorization
let variance1 = var vector1 // calculate variance of a vector
let variance2 = var matrix1 ColumnAxis // calculate variance of each column of a matrix
let vector4 = log((vector2.AsExpr + 3.0) .^ 3) |> eval // avoid creating temporary vectors
MklControl.SetMaxThreads(4) // set MKL to use up to 4 threads

(** And the variable `matrix1` has the following value: *)
(*** include-value: matrix1 ***)

(**

Documentation
-----------------------

The library comes with a comprehensive library guide: 

 * [BoolVector](BoolVector.html) introduces BoolVector type and related functions
 * [Vector](Vector.html) introduces Vector type and related functions
 * [BoolMatrix](BoolMatrix.html) introduces BoolMatrix type and related functions
 * [Matrix](Matrix.html) introduces Matrix type and related functions
 * [Linear Algebra](LinearAlgebra.html) introduces matrix factorizations and linear solvers
 * [Vector and Matrix Functions](VectorAndMatrixFunctions.html) introduces vector and matrix functions
 * [Random Number Generators](RandomNumberGenerators.html) introduces random number generators
 * [Basic Stats](BasicStats.html) introduces basic statistical functions
 * [Vector and Matrix Expressions](VectorAndMatrixExpressions.html) introduces vector and matrix expressions
 * [FCor vs Matlab: Pi Monte Carlo](PiMonteCarlo.html) compares Matlab and FCor implementations of Pi MC algorithm
 * [FCor vs Matlab: American Option Binomial Pricing](BinomialOption.html) compares Matlab and FCor implementations of binomial option pricing
 * [FCor vs Matlab: American Option LS MC](LSMOption.html) compares Matlab and FCor implementations of Least Squares MC option pricing

   
Contributing and copyright
--------------------------

Copyright (c) 2007-2015 StatFactory Ltd

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation.

The library source code is available under MIT/X11 license. For more information see the 
[License file][license] in the GitHub repository.

The library uses Intel Math Kernel Library for high performance. This software is not open source and therefore FCor distribution on Nuget has its own license. 

  [content]: https://github.com/Statfactory/FCor/tree/master/docs/content
  [gh]: https://github.com/Statfactory/FCor
  [issues]: https://github.com/Statfactory/FCor/issues
  [readme]: https://github.com/Statfactory/FCor/blob/master/README.md
  [license]: https://github.com/Statfactory/FCor/blob/master/LICENSE.txt

*)
