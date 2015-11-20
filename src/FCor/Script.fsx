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


//open FCor.CsvProvider
//[<LiteralAttribute>]
//let pathPredictedCsv = @"C:\Users\Adam Mlocek\Development\FCore\bin\GLM\gammatestpredict.csv"
//type NewDataFrame = CsvDataFrame<pathPredictedCsv>
//let newDF = new NewDataFrame()

let N = 500000
let rng = new MT19937Rng()
let rnd = new Random()
MklControl.SetMaxThreads 1

//let v20 = rand rng N : Vector
//let v100 = rand rng N : Vector
//let X = rand rng N : Vector
//let gamRnd = gammaRnd rng 0.0 1.0 1.0 N : Vector
//let xbeta = eval (2.2 + v20.AsExpr + v100 + 3.3 * X.AsExpr)
//let Y = eval (gamRnd.AsExpr .* exp(xbeta.AsExpr))
//
//let XStorage = new CovariateStorageFloat32()
//XStorage.SetSlice(0L, X.ToArray() |> Array.map float32)
//let XVar = new Covariate("X", XStorage)
//let AStorage = new FactorStorage()
//let Alevels = Array.init N (fun i -> sprintf "A%d" (v20.[i] * 20.0 |> floor |> int))
//AStorage.SetSlice(0L, Alevels)
//let AVar = new Factor("A", AStorage)
//let BStorage = new FactorStorage()
//BStorage.SetSlice(0L, Array.init N (fun i -> sprintf "B%d" (v100.[i] * 100.0 |> floor |> int)))
//let BVar = new Factor("B", BStorage)
//let YStorage = new CovariateStorageFloat32()
//YStorage.SetSlice(0L, Y.ToArray() |> Array.map float32)
//let YVar = new Covariate("Y", YStorage)
#time
let pathCsv = @"C:\Users\Adam Mlocek\Development\FCore\bin\GLM\gammatest.csv"


//Glm.toCsv [StatVariable.Factor(AVar);StatVariable.Factor(BVar);StatVariable.Covariate(XVar);StatVariable.Covariate(YVar)] pathCsv

let statVars = Glm.importCsv pathCsv false
let A = statVars.[0].AsFactor
let B = statVars.[1].AsFactor
let X = statVars.[2].AsCovariate
let Y = statVars.[3].AsCovariate

let model = Glm.fitModel Y.AsExpr (X + A + B + (2.0 * X.AsExpr)) true Gamma Ln 10000 100 1e-8

let fitted = Glm.fitted model (new DataFrame(statVars))
let res = (Y.AsExpr - fitted.AsExpr).AsCovariate


//let X = rand rng 1000000 : Vector
//let e = normRnd rng 0.0 1.0 1000000 : Vector
//let Y = 2.2 + 3.3 * X + e
//let XVar = new Covariate("X", new CovariateStorage(X))
//let YVar = new Covariate("Y", new CovariateStorage(Y))
//#time
//let glm = Glm.fitModel YVar.AsExpr [NumericalPredictor XVar.AsExpr] true Gaussian Id 1000000 50 1e-10





