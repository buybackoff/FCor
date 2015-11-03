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

let N = 5000000
let rng = new MT19937Rng()
let rnd = new Random()
MklControl.SetMaxThreads 1
let rnd20 = Array.init N (fun i -> rnd.Next(20))
let rnd100 = Array.init N (fun i -> rnd.Next(100))
let ACol = Array.init N (fun i -> sprintf "A%d" rnd20.[i])
let BCol = Array.init N (fun i -> sprintf "B%d" rnd100.[i])
let X = rand rng N : Vector
let gamRnd = gammaRnd rng 0.0 1.0 1.0 N : Vector
let xbeta = (2.2 + (new Vector(rnd20 |> Array.map float)) / 20.0 + (new Vector(rnd100 |> Array.map float)) / 100.0) + 3.3 * X
let Y = gamRnd .* exp(xbeta)

let XVar = new Covariate("X", new CovariateStorage(X))
let AStorage = new FactorStorage(N)
AStorage.SetSlice(0, ACol)
let AVar = new Factor("A", AStorage)
let BStorage = new FactorStorage(N)
BStorage.SetSlice(0, BCol)
let BVar = new Factor("B", BStorage)
let YVar = new Covariate("Y", new CovariateStorage(Y))
#time
let glm = Glm.fitModel YVar.AsExpr (AVar + BVar + XVar) true Gamma Ln N 50 1e-10
let pp = glm |> Option.map fst |> Option.map (fun prms -> prms |> List.filter (fun p -> p.Predictor = "X"))

//let X = rand rng 1000000 : Vector
//let e = normRnd rng 0.0 1.0 1000000 : Vector
//let Y = 2.2 + 3.3 * X + e
//let XVar = new Covariate("X", new CovariateStorage(X))
//let YVar = new Covariate("Y", new CovariateStorage(Y))
//#time
//let glm = Glm.fitModel YVar.AsExpr [NumericalPredictor XVar.AsExpr] true Gaussian Id 1000000 50 1e-10





