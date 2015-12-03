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
open FCor.StatModels
open FCor.BasicStats

module Predictor =

    [<Fact>]
    let ``Create factor from string array`` () =
        let data = [|"A1";"A2";"A0";"A0";"A1"|]
        let factor : Factor = !!data
        factor.Length |> should equal 5L
        factor.Cardinality |> should equal 3
        factor.AsSeq |> Seq.toArray |> should equal [|(0us,"A1");(1us,"A2");(2us,"A0");(2us,"A0");(0us,"A1")|]
        Array.init factor.Cardinality (fun i -> factor.Level(i)) |> should equal [|"A1";"A2";"A0"|]

    [<Fact>]
    let ``Get factor stats`` () =
        let data = [|"A1";"A2";"A0";"A0";"A1";"";"N/A";"A1";"A0";"A2"|]
        let factor : Factor = !!data
        let stats = factor.GetStats()
        stats.ObsCount |> should equal 10L
        stats.MissingFreq |> should equal 2L
        stats.``Missing%`` |> should equal 0.2
        stats.LevelStats |> should equal [|{Level = "A1"; Freq = 3L; ``Freq%`` = 0.3}
                                           {Level = "A2"; Freq = 2L; ``Freq%`` = 0.2}
                                           {Level = "A0"; Freq = 3L; ``Freq%`` = 0.3}
                                           {Level = ""; Freq = 1L; ``Freq%`` = 0.1}
                                           {Level = "N/A"; Freq = 1L; ``Freq%`` = 0.1}
                                         |]

    [<Fact>]
    let ``Create covariate from float array`` () =
        let data = [|1.0..10.0|]
        let cov : Covariate = !!data
        cov.Length |> should equal 10L
        cov.AsSeq |> Seq.toArray |> should equal data

    [<Fact>]
    let ``Get cov stats`` () =
        let data = [|1.0..10.0|]
        let dataNaN = data |> Array.append [|Double.NaN|]
        let cov : Covariate = !!dataNaN
        let stats = cov.GetStats()
        stats.ObsCount |> should equal 11L
        stats.NaNCount |> should equal 1L
        stats.``NaN%`` |> should equal (1.0/11.0)
        stats.Min |> should equal 1.0
        stats.Max |> should equal 10.0
        stats.Mean |> should equal ((!!data:Vector) |> mean)
        stats.Std |> should equal ((!!data:Vector) |> var |> sqrt)

    [<Fact>]
    let ``Rename factor`` () =
        let data = [|"A1";"A2";"A0";"A0";"A1"|]
        let factor : Factor = !!data
        factor.Name |> should equal "Factor"
        let factor2 = factor |>> "askfsalkj"
        factor2.Name |> should equal "askfsalkj"

    [<Fact>]
    let ``Rename factor levels`` () =
        let data = [|"A1";"A2";"A0";"A0";"A1"|]
        let factor : Factor = !!data
        let factor2 = factor |>> (fun (level : string) -> sprintf "B%s" level)
        factor2.AsFactor.Cardinality |> should equal factor.Cardinality
        Array.init factor2.AsFactor.Cardinality (fun i -> factor2.AsFactor.Level(i)) |> should equal (Array.init factor.Cardinality (fun i -> sprintf "B%s" (factor.Level(i))))

    [<Fact>]
    let ``Merge factor levels`` () =
        let data = [|"A1";"A2";"A0";"A0";"A1"|]
        let factor : Factor = !!data
        let factor2 = factor |>> ["A0";"A1";"A3"]
        factor2.AsFactor.Cardinality |> should equal 2
        factor2.AsFactor.AsSeq |> Seq.toArray |> should equal [|(0us,"A0|A1|A3");(1us,"A2");(0us,"A0|A1|A3");(0us,"A0|A1|A3");(0us,"A0|A1|A3")|]

    [<Fact>]
    let ``Cross factors`` () =
        let data1 = [|"A1";"A2";"A0";"A0";"A1"|]
        let factor1 : Factor = !!data1
        let data2 = [|"B1";"B2";"B0";"B0";"B1"|]
        let factor2 : Factor = !!data2
        let factor3 = factor1 .* factor2
        factor3.AsFactor.Cardinality |> should equal (factor1.Cardinality * factor2.Cardinality)
        Array.init factor3.AsFactor.Cardinality (fun i -> factor3.AsFactor.Level(i)) |> should equal [|"A1*B1";"A2*B1";"A0*B1";"A1*B2";"A2*B2";"A0*B2";"A1*B0";"A2*B0";"A0*B0";|]
        factor3.AsFactor.AsSeq |> Seq.toArray |> should equal [|(0us,"A1*B1");(4us,"A2*B2");(8us,"A0*B0");(8us,"A0*B0");(0us,"A1*B1")|]

    [<Fact>]
    let ``Cut cov`` () =
        let data = [|1.1..3.1|]
        let cov : Covariate = !!data
        let factor = cov |>> [|1.0..3.0|]
        factor.AsFactor.Cardinality |> should equal 3
        Array.init factor.AsFactor.Cardinality (fun i -> factor.AsFactor.Level(i)) |> should equal [|"[1,2)";"[2,3]";""|] 
        factor.AsFactor.AsSeq |> Seq.toArray |> should equal [|(0us,"[1,2)");(1us,"[2,3]");(2us,"")|]

    [<Fact>]
    let ``Int cov`` () =
        let data = [|1.1..3.1|]
        let cov : Covariate = !!data
        let factor = cov |> floor |>> [|1..3|]
        factor.AsFactor.Cardinality |> should equal 4
        Array.init factor.AsFactor.Cardinality (fun i -> factor.AsFactor.Level(i)) |> should equal [|"1";"2";"3";""|] 
        factor.AsFactor.AsSeq |> Seq.toArray |> should equal [|(0us,"1");(1us,"2");(2us,"3")|]

    [<Fact>]
    let ``Permute factor levels`` () =
        let data = [|"A1";"A2";"A0";"A0";"A1"|]
        let factor : Factor = !!data
        let permutation = [|(1,"A0");(2,"A1");(0,"A2")|]
        let factor2 = factor |>> permutation
        factor2.AsFactor.Cardinality |> should equal 3
        Array.init factor2.AsFactor.Cardinality (fun i -> factor2.AsFactor.Level(i)) |> should equal [|"A2";"A0";"A1"|]
        factor2.AsFactor.AsSeq |> Seq.toArray |> should equal [|(1us,"A0");(2us,"A1");(0us,"A2");(0us,"A2");(1us,"A0")|]

    [<Fact>]
    let ``Rename covariate`` () =
        let data = [|1.1..3.1|]
        let cov : Covariate = !!data
        cov.Name |> should equal "Covariate"
        let cov2 = cov |>> "askfsalkj"
        cov2.Name |> should equal "askfsalkj"

    [<Fact>]
    let ``Parse factor`` () =
        let data = [|"1";"2";"0";"0.2";"3.3"|]
        let factor : Factor = !!data
        let cov = factor |>> (fun (level : string) -> match Double.TryParse(level) with | (true, v) -> v | _ -> Double.NaN)
        cov.AsCovariate.AsSeq |> Seq.toArray |> should equal [|1.0;2.0;0.0;0.2;3.3|]

    [<Fact>]
    let ``Binomial factor`` () =
        let data = [|"A1";"A2";"A0";"A0";"A1"|]
        let factor : Factor = !!data
        let cov = factor |>> (fun (level : string) -> level = "A1" || level = "A2")
        cov.AsCovariate.AsSeq |> Seq.toArray |> should equal [|1.0;1.0;0.0;0.0;1.0|]








        






