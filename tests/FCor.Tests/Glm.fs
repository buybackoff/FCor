namespace FCor.Tests

open FCor
open FCor.Math
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

        let v20 = rand rng N : Vector
        let v100 = rand rng N : Vector
        let X = rand rng N : Vector
        let gamRnd = gammaRnd rng 0.0 1.0 1.0 N : Vector
        let xbeta = eval (2.2 + v20.AsExpr + v100 + 3.3 * X.AsExpr)
        let Y = gamRnd .* exp(xbeta)

        let XVar = new Covariate("X", new CovariateStorage(X))
        let AStorage = new FactorStorage(seq{0..N-1} |> Seq.map (fun i -> sprintf "A%d" (v20.[i] * 20.0 |> floor |> int)))
        let AVar = new Factor("A", AStorage)
        let BStorage = new FactorStorage(seq{0..N-1} |> Seq.map (fun i -> sprintf "B%d" (v100.[i] * 100.0 |> floor |> int)))
        let BVar = new Factor("B", BStorage)
        let YVar = new Covariate("Y", new CovariateStorage(Y))
        let glm = Glm.fitModel YVar.AsExpr (AVar + BVar + XVar) true Gamma Ln 15000 50 1e-6
        ()

