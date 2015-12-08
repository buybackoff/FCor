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
open FCor.BasicStats
open FCor.CsvProvider

module DataFrame =
    type GammaCsv = CsvDataFrame< "gamma.csv" >

    [<Fact>]
    let ``Load csv via type provider`` () =
        let df = new GammaCsv()
        df.mfg.Cardinality |> should equal 2
        Array.init df.mfg.Cardinality (fun i -> df.mfg.Level(i)) |> should equal [|"A";"B"|]
        df.lifetime.Length |> should equal 201L
        df.mfg.Length |> should equal 201L

    [<Fact>]
    let ``Get dataframe`` () =
        let df = (new GammaCsv()).DataFrame
        let mfg = df.["mfg"].AsFactor
        let lifetime = df.["lifetime"].AsCovariate
        lifetime.Length |> should equal 201L
        mfg.Length |> should equal 201L
        Array.init mfg.Cardinality (fun i -> mfg.Level(i)) |> should equal [|"A";"B"|]

    [<Fact>]
    let ``Gamma log glm 1 factor`` () =
        let df = new GammaCsv()
        let model = glm (df.lifetime <~> df.mfg) true Gamma Ln 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 201L
                goodnessOfFit.Deviance |> should (equalWithin 1e-4) 287.0591
                goodnessOfFit.Deviance / N_dof  |> should (equalWithin 1e-4) 1.4425
                goodnessOfFit.PearsonChi |> should (equalWithin 1e-3) 211.687
                goodnessOfFit.PearsonChi / N_dof  |> should (equalWithin 1e-4) 1.0638
                goodnessOfFit.Deviance / phi |> should (equalWithin 1e-4) 237.5335
                goodnessOfFit.PearsonChi / phi |> should (equalWithin 1e-3) 175.165
                goodnessOfFit.LogLikehood |> should (equalWithin 1e-4) -1432.4177
                goodnessOfFit.MLScale |> should (equalWithin 1e-4) 0.8275
            | None -> ()

        

