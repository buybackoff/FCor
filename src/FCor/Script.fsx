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
open FCor.StatModels
open Overloading
open BasicStats

open FCor.CsvProvider
type Csv = CsvDataFrame< @"C:\Users\Adam Mlocek\Development\FCore\tests\FCor.StatModels.Tests\houses.csv", Separator = " " >
let __ = new Csv()
let price = __.P |>> "Price"
let size = __.S |>> "Size"
let beds = __.Be |>> [1..5]  |>> "Beds"
let baths = __.Ba |>> [1..3] |>> "Baths"
let isNew = __.New |>> [0..1] |>> "IsNew"
let model = glm (price <~> beds + baths + isNew + beds * baths) true Gamma Log 50 1e-9
(beds .* baths).AsFactor.GetStats()
let fitted = model.Predict()




//type BinomCsv = CsvDataFrame< @"C:\Users\Adam Mlocek\Development\FCore\tests\FCor.StatModels.Tests\binomial.csv", Separator = " " >
//let df = new BinomCsv()
//let model = glm (df.r / df.n <~> df.x + df.drug) true Binomial Logit 50 1e-9



//open FCor.CsvProvider
//[<LiteralAttribute>]
//let pathPredictedCsv = @"C:\Users\Adam Mlocek\Development\FCore\bin\GLM\gammatest250K.csv"
//type NewDataFrame = CsvDataFrame<HasHeaders = true, Filename = pathPredictedCsv>
//let newDF = new NewDataFrame()
//let A = newDF.A
//let stats = A.GetStats()

//let N = 25000000
//let rng = new MT19937Rng()
//let rnd = new Random()
//MklControl.SetMaxThreads 1
//
//let v20 = rand rng N : Vector
//let v100 = rand rng N : Vector
//let X = rand rng N : Vector
//let gamRnd = gammaRnd rng 0.0 1.0 1.0 N : Vector
//let Y = eval (gamRnd.AsExpr .* exp(2.2 + v20.AsExpr + v100 + 3.3 * X.AsExpr))
//
//let XVar = (!!X : Covariate) |>> "X"
//let YVar = (!!Y : Covariate) |>> "Y"
//let AVar : FactorExpr = (20.0 * v20) |> floor |> (!!) |>> [|0..19|] |>> (fun level -> sprintf "A%s" level) |>> "A"
//let BVar : FactorExpr = (100.0 * v100) |> floor |> (!!) |>> [|0..99|] |>> (fun level -> sprintf "B%s" level) |>> "B"

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


//Glm.toCsv [StatVariable.Factor(AVar.AsFactor);StatVariable.Factor(BVar.AsFactor);StatVariable.Covariate(XVar.AsCovariate);StatVariable.Covariate(YVar.AsCovariate)] pathCsv

let statVars = Glm.importCsv pathCsv [|','|] [||] true false
let A = statVars.[0].AsFactor
let B = statVars.[1].AsFactor
let X = statVars.[2].AsCovariate
let Y = statVars.[3].AsCovariate


let model = glm (Y <~> A + B + X) true Gamma Ln 50 1e-8
//
//
//let fitted = model.Predict(new DataFrame(statVars))
//let resStats = fitted.GetStats()

//let X = rand rng 1000000 : Vector
//let e = normRnd rng 0.0 1.0 1000000 : Vector
//let Y = 2.2 + 3.3 * X + e
//let XVar = new Covariate("X", new CovariateStorage(X))
//let YVar = new Covariate("Y", new CovariateStorage(Y))
//#time
//let glm = Glm.fitModel YVar.AsExpr [NumericalPredictor XVar.AsExpr] true Gaussian Id 1000000 50 1e-10





