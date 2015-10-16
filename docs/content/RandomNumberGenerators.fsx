(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FCore"
(**
Random Number Generators
========================
FCore supports all Basic Random Number Generators from MKL:

- `MCG31` 31-bit multiplicative congruential pseudorandom generator
- `R250` generalized feedback shift register pseudorandom generator
- `MRG32K3A` combined multiple recursive pseudorandom generator with two components of order 3
- `MCG59` 59-bit multiplicative congruential pseudorandom generator
- `WH` set of 273 Wichmann-Hill combined multiplicative congruential pseudorandom generators
- `MT19937` Mersenne Twister pseudorandom number generator
- `SFMT19937` SIMD-oriented Fast Mersenne Twister pseudorandom number generator
- `MT2203` set of 6024 Mersenne Twister pseudorandom number generators
- `SOBOL` 32-bit Gray code-based generator producing low-discrepancy sequences for dimensions 1 ≤ s ≤ 40
- `NIEDERR` 32-bit Gray code-based generator producing low-discrepancy sequences for dimensions 1 ≤ s ≤ 318

You can also generate random vectors and matrices from a range of continuous and discreet distributions.

See [VSL Notes](https://software.intel.com/sites/default/files/managed/78/55/vslnotes.pdf) for technical details about random number generators in MKL.
*)
#r "FCore.dll"

open FCore
open System
open FCore.ExplicitConversion
open FCore.Math
open FCore.Random
(**
Basic Random Number Generators
--------------------------------
Each Basic Random Number Generator can be created with a constructor:
*)
let rng1 = new MCG31Rng() // seed will be created deterministically by MKL, see VSL Notes for details
(**
You can pass a seed to the constructor (as uint32 or uint32[]): 
*)
let rng2 = new MCG31Rng(1u)
let rng3 = new MCG31Rng([|1u;2u|])
(**
`SOBOL` and `NIEDERR` take dimension as constructor parameter (default = 1):
*)
let rng4 = new SOBOLRng(10) 
(**
Some generators support `SkipAhead` or `LeapFrog` generation, see VSL Notes for details:
*)
rng1.SkipAhead(10000) // skip 10000 elements
rng1.LeapFrog(2, 10) // start at element 3, return every 10th
(**
Once you have a basic generator you can use `rand` function to generate numbers in [0,1] interval:
*)
let v1 : Vector = rand rng1 100 // create a random vector of length 100, note we have to help compiler with type annotation
let m1 = rand rng1 100 200 // create a random matrix 100x200
(**
You can also use F# partial application and create a vector or matrix generator function:
*)
let randV : (int -> Vector) = rand rng1
let randM : (int -> int -> Matrix) = rand rng1
let v3 = randV 200
let m2 = randM 300 400
(**
Distribution Generators
-----------------------
Each FCore distribution generation function is overloaded: it can return a vector or a matrix.

For vectors, you need to pass as arguments: basic rng, distribution paramaters and vector length as `int` or `int64`.

For matrices, you need to pass as arguments: basic rng, distribution paramaters and matrix size as `int` `int` or `int64` `int64`:
*)
let v4 : Vector = normRnd rng1 0.0 1.0 300 // normal distribution, mu=0.0, sigma=1.0, length=300
let m3 = normRnd rng1 0.0 1.0 300L 400L // normal distribution, mu=0.0, sigma=1.0, rowCount=300, colCount=400
(**
The following distribution functions are available:

- `unifRnd rng a b` uniform distribution between `a` and `b` 
- `normRnd rng mu sigma` normal distribution with given mean `mu` and standard deviation `sigma`
- `exponRnd rng a beta` exponential distribution with given displacement `a` and scale `beta`
- `laplaceRnd rng a beta` Laplace distribution with given mean `a` and scale `beta`
- `weibullRnd rng a alpha beta` Weibull distribution with given displacement `a`, shape `alpha` and scale `beta`
- `cauchyRnd rng a beta` Cauchy distribution with given displacement `a` and scale `beta`
- `rayleighRnd rng a beta` Rayleigh distribution with given displacement `a` and scale `beta`
- `lognormRnd rng mu sigma a beta` lognormal distribution with given mean `mu`, `sigma` standard deviation, displacement `a` and scale `beta`
- `gumbelRnd rng a beta` Gumbel distribution with given displacement `a` and scale `beta`
- `gammaRnd rng a alpha beta` Gamma distribution with given displacement `a`, shape `alpha` and scale `beta`
- `betaRnd rng p q a beta` Beta distribution with given displacement `a`, shape parameters: `p`, `q` and scale `beta`
- `bernRnd rng p` Bernoulli distribution with success probability `p`
- `geomRnd rng p` geometric distribution with success probability `p`
- `binomRnd rng n p` Binomial distribution with success probability `p` and number of trials `n`
- `hyperGeomRnd rng l s m` hypergeometric distribution with lot size `l`, sampling size `s` and number of marked elements in lot `m`
- `poissonRnd rng lambda` Poisson distribution with given `lambda`
- `negbinomRnd rng a p` negative binomial distribution with parameters `a` and `p`
*)

