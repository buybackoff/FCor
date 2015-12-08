namespace FCor.StatModels.Tests

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
open FCor.StatModels

//module Glm =
//
//    [<Fact>]
//    let ``Run Glm`` () =
//        MklControl.SetMaxThreads 1
//        let pathCsv = @"C:\Users\Adam Mlocek\Development\FCore\bin\GLM\gammatest.csv"
//        let statVars = Glm.importCsv pathCsv [|','|] [||] true false
//        let A = statVars.[0].AsFactor
//        let B = statVars.[1].AsFactor
//        let X = statVars.[2].AsCovariate
//        let Y = statVars.[3].AsCovariate
//        let A' = A |>> (fun (level : string) -> let d = int <| level.Substring(1) in sprintf "A%d" (d % 3))
//        let B' = B |>> (fun (level : string) -> let d = int <| level.Substring(1) in sprintf "B%d" (d % 4))
//        let glm = glm (Y <~> A' + A' * B') false Gamma Ln 50 1e-6
//        ()

