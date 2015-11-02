#r @".\bin\release\FCor.dll"
open FCor
open FCor.ExplicitConversion
open FCor.Math
open FCor.LinearAlgebra
open System
open System.IO
open System.Runtime.InteropServices
open System.Collections.Generic
open FCor.Random

open Overloading
open BasicStats

let rng = new MT19937Rng()
MklControl.SetMaxThreads 4

let X = rand rng 1000000 : Vector
let e = normRnd rng 0.0 1.0 1000000 : Vector
let Y = 2.2 + 3.3 * X + e
let XVar = new Covariate("X", new CovariateStorage(X))
let YVar = new Covariate("Y", new CovariateStorage(Y))
#time
let glm = Glm.fitModel YVar.AsExpr [NumericalPredictor XVar.AsExpr] true Gaussian Id 1000000 50 1e-10





