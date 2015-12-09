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

module Glm =

    type GammaCsv = CsvDataFrame< "gamma.csv" > // SAS
    type BinomialCsv = CsvDataFrame< "binomial.csv" , Separator = " "> // SAS
    type BeetleCsv = CsvDataFrame< "beetle.csv" , Separator = " "> // Dobson
    type BinaryCsv = CsvDataFrame< "binary.csv" > // "http://www.ats.ucla.edu/stat/data/binary.csv"
    type NormalCsv = CsvDataFrame< "normal.csv", Separator = " " > // SAS
    type PoissonCsv = CsvDataFrame< "poisson_sim.csv" > // "http://www.ats.ucla.edu/stat/data/poisson_sim.csv"

    [<Fact>]
    let ``Gamma log 1 factor`` () =
        let df = new GammaCsv()
        let model = glm (df.lifetime <~> df.mfg) true Gamma Log 50 1e-9
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

    [<Fact>]
    let ``Gamma inverse 1 factor`` () =
        let df = new GammaCsv()
        let model = glm (df.lifetime <~> df.mfg) true Gamma Inverse 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                goodnessOfFit.ValidObsCount |> should equal 201L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 287.06
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-4) 468.7444
        pred.[90] |> should (equalWithin 1e-4) 459.5135

    [<Fact>]
    let ``Gamma id 1 factor`` () =
        let df = new GammaCsv()
        let model = glm (df.lifetime <~> df.mfg) true Gamma Id 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                goodnessOfFit.ValidObsCount |> should equal 201L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 287.06
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-2) 468.7444
        pred.[90] |> should (equalWithin 1e-4) 459.5135

    [<Fact>]
    let ``Binomial r/n logit 1 covariate 1 factor`` () =
        let __ = new BinomialCsv()
        let model = glm (__.r / __.n <~> __.x +  __.drug) true Binomial Logit 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 18L
                goodnessOfFit.Deviance |> should (equalWithin 1e-4) 5.2751
                goodnessOfFit.Deviance / N_dof  |> should (equalWithin 1e-4) 0.4396
                goodnessOfFit.PearsonChi |> should (equalWithin 1e-3) 4.5133
                goodnessOfFit.PearsonChi / N_dof  |> should (equalWithin 1e-4) 0.3761
                goodnessOfFit.Deviance / phi |> should (equalWithin 1e-4) 5.2751
                goodnessOfFit.PearsonChi / phi |> should (equalWithin 1e-3) 4.5133
                goodnessOfFit.LogLikehood |> should (equalWithin 1e-4) -114.7732
                goodnessOfFit.MLScale |> should (equalWithin 1e-4) 1.0
            | None -> ()

    [<Fact>]
    let ``Binomial r/n logit 1 covariate`` () =
        let __ = new BeetleCsv()
        let model = glm (__.y / __.n <~> __.x) true Binomial Logit 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 8L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 11.23
                goodnessOfFit.LogLikehood |> should (equalWithin 1e-3) -186.235
                model.Beta.[0] |> should (equalWithin 1e-3) -60.717
                model.Beta.[1] |> should (equalWithin 1e-3) 34.270
            | None -> ()

    [<Fact>]
    let ``Binomial r/n probit 1 covariate`` () =
        let __ = new BeetleCsv()
        let model = glm (__.y / __.n <~> __.x) true Binomial Probit 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 8L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 10.12
            | None -> ()
        let pred : Vector =  !!(model.Predict()  * __.n).AsCovariate
        pred.[1] |> should (equalWithin 1e-2) 10.72
        pred.[7] |> should (equalWithin 1e-2) 59.23

    [<Fact>]
    let ``Binomial r/n cloglog 1 covariate`` () =
        let __ = new BeetleCsv()
        let model = glm (__.y / __.n <~> __.x) true Binomial CLogLog 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 8L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 3.45
            | None -> ()
        let pred : Vector =  !!(model.Predict()  * __.n).AsCovariate
        pred.[0] |> should (equalWithin 1e-2) 5.59
        pred.[7] |> should (equalWithin 1e-2) 59.95

    [<Fact>]
    let ``Binary logit 2 covariates 1 factor`` () =
        let __ = new BinaryCsv()
        let model = glm (__.admit <~> __.gre + __.gpa + (__.rank |>> [|1..4|])) true Binomial Logit 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                goodnessOfFit.ValidObsCount |> should equal 400L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 458.52
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-4) 0.1726
        pred.[9] |> should (equalWithin 1e-4) 0.5178

    [<Fact>]
    let ``Gaussian log 1 covariate`` () =
        let __ = new NormalCsv()
        let model = glm (__.y <~> __.x) true Gaussian Log 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 16L
                goodnessOfFit.Deviance |> should (equalWithin 1e-4) 52.3000
                goodnessOfFit.Deviance / N_dof  |> should (equalWithin 1e-4) 3.7357
                goodnessOfFit.PearsonChi |> should (equalWithin 1e-4) 52.3000
                goodnessOfFit.PearsonChi / N_dof  |> should (equalWithin 1e-4) 3.7357
                goodnessOfFit.Deviance / phi |> should (equalWithin 1e-4) 16.0000
                goodnessOfFit.PearsonChi / phi |> should (equalWithin 1e-4) 16.0000
                goodnessOfFit.LogLikehood |> should (equalWithin 1e-4) -32.1783
                goodnessOfFit.MLScale |> should (equalWithin 1e-4) 1.8080
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-4) 5.5921
        pred.[3] |> should (equalWithin 1e-4) 7.9324

    [<Fact>]
    let ``Gaussian id 1 covariate`` () =
        let __ = new NormalCsv()
        let model = glm (__.y <~> __.x) true Gaussian Id 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 16L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2)  179.41
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-4) 3.6295
        pred.[4] |> should (equalWithin 1e-4) 8.6533

    [<Fact>]
    let ``Gaussian inverse 1 covariate`` () =
        let __ = new NormalCsv()
        let model = glm (__.y <~> __.x) true Gaussian Inverse 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                let N_dof = float (goodnessOfFit.ValidObsCount - int64 goodnessOfFit.DoF)
                let phi = goodnessOfFit.MLPhi
                goodnessOfFit.ValidObsCount |> should equal 16L
                goodnessOfFit.Deviance |> should (equalWithin 1e-3) 77.088
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-4) 7.9704
        pred.[5] |> should (equalWithin 1e-4) 9.3920

    [<Fact>]
    let ``Poisson log 1 covariate 1 factor`` () =
        let __ = new PoissonCsv()
        let model = glm (__.num_awards <~> (__.prog |>> [|1..3|]) + __.math) true Poisson Log 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                goodnessOfFit.ValidObsCount |> should equal 200L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 189.45
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-5) 0.13519
        pred.[9] |> should (equalWithin 1e-5) 0.19199

    [<Fact>]
    let ``Poisson inverse 1 covariate 1 factor`` () =
        let __ = new PoissonCsv()
        let model = glm (__.num_awards <~> (__.prog |>> [|1..3|]) + __.math) true Poisson Inverse 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                goodnessOfFit.ValidObsCount |> should equal 200L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 200.34
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-5) 0.24106
        pred.[5] |> should (equalWithin 1e-5) 0.18658

    [<Fact>]
    let ``Poisson id 1 factor`` () =
        let __ = new PoissonCsv()
        let model = glm (__.num_awards <~> (__.prog |>> [|1..3|])) true Poisson Id 50 1e-9
        model.GoodnessOfFit.IsSome |> should be True
        match model.GoodnessOfFit with
            | Some(goodnessOfFit) ->
                goodnessOfFit.ValidObsCount |> should equal 200L
                goodnessOfFit.Deviance |> should (equalWithin 1e-2) 234.46
            | None -> ()
        let pred : Vector =  !!model.Predict()
        pred.[0] |> should (equalWithin 1e-2) 0.24
        pred.[1] |> should (equalWithin 1e-2) 0.2




