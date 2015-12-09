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


        

