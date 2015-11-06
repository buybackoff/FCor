namespace FCor.Tests

open FCor
open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit
open FsUnit.Xunit
open System
open FCor.Random
open FCor.ExplicitConversion

module Glm =

    [<Fact>]
    let ``Run Glm`` () =
//        let rng = new MT19937Rng()
//        let X = rand rng 1000000 : Vector
//        let e = normRnd rng 0.0 1.0 1000000 : Vector
//        let Y = 2.2 + 3.3 * X + e
//        let XVar = new Covariate("X", new CovariateStorage(X))
//        let YVar = new Covariate("Y", new CovariateStorage(Y))
//        let glm = Glm.fitModel YVar.AsExpr [NumericalPredictor XVar.AsExpr] true Gaussian Id 1000000 50 1e-10
//        ()
        let N = 1000000
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
        let glm = Glm.fitModel YVar.AsExpr (AVar + BVar + XVar) true Gamma Ln 100000 50 1e-10
        ()

