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

let arr = Array.init 257 (fun i -> sprintf "A%d" (i%257))
let ss = seq{1..100000000} |> Seq.map (fun i -> arr.[i%257])
#time
let SS = new Factor("A", new FactorStorage(ss))

let N = 5000000
let rng = new MT19937Rng()
let rnd = new Random()
MklControl.SetMaxThreads 1

let v20 = rand rng N : Vector
let v100 = rand rng N : Vector
let X = rand rng N : Vector
let gamRnd = gammaRnd rng 0.0 1.0 1.0 N : Vector
let xbeta = eval (2.2 + v20.AsExpr + v100 + 3.3 * X.AsExpr)
let Y = eval (gamRnd.AsExpr .* exp(xbeta.AsExpr))

let XVar = new Covariate("X", new CovariateStorage(X))
let AStorage = new FactorStorage(seq{0..N-1} |> Seq.map (fun i -> sprintf "A%d" (v20.[i] * 20.0 |> floor |> int)))
let AVar = new Factor("A", AStorage)
let BStorage = new FactorStorage(seq{0..N-1} |> Seq.map (fun i -> sprintf "B%d" (v100.[i] * 100.0 |> floor |> int)))
let BVar = new Factor("B", BStorage)
let YVar = new Covariate("Y", new CovariateStorage(Y))
#time
Glm.toCsv [StatVariable.Factor(AVar); StatVariable.Factor(BVar); StatVariable.Covariate(XVar);StatVariable.Covariate(YVar)] @"C:\Users\Adam Mlocek\Development\FCore\bin\GLM\gammatest.csv"


let glm = Glm.fitModel YVar.AsExpr (AVar + BVar + XVar) true Gamma Ln 15000 50 1e-6
let pp = glm |> Option.map fst |> Option.map (fun prms -> prms |> List.filter (fun p -> p.Predictor = "X"))
glm |> Option.map snd

//let X = rand rng 1000000 : Vector
//let e = normRnd rng 0.0 1.0 1000000 : Vector
//let Y = 2.2 + 3.3 * X + e
//let XVar = new Covariate("X", new CovariateStorage(X))
//let YVar = new Covariate("Y", new CovariateStorage(Y))
//#time
//let glm = Glm.fitModel YVar.AsExpr [NumericalPredictor XVar.AsExpr] true Gaussian Id 1000000 50 1e-10





