namespace FCor.Tests

open FCor
open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit
open FsUnit.Xunit
open System
open FCor.Random

module Glm =

    [<Fact>]
    let ``Run Glm`` () =
        let rng = new MT19937Rng()
        let X = rand rng 1000000 : Vector
        let e = normRnd rng 0.0 1.0 1000000 : Vector
        let Y = 2.2 + 3.3 * X + e
        let XVar = new Covariate("X", new CovariateStorage(X))
        let YVar = new Covariate("Y", new CovariateStorage(Y))
        let glm = Glm.fitModel YVar.AsExpr [NumericalPredictor XVar.AsExpr] true Gaussian Id 1000000 50 1e-10
        ()

