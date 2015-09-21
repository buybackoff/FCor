namespace FCore.MatlabTests

open FCore
open FCore.Math
open FCore.BasicStats
open FCore.Random
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module VectorBasicStats =

    let app = new MLAppClass()
    do app.Visible <- 0

    let rng = new MT19937Rng()
    let rnd = new Random()

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[]) (y :float[])  = epsEqualArray x y epsEqualFloat eps

    let regDouble x =
        if Double.IsNaN(x) || Double.IsNegativeInfinity(x) || Double.IsPositiveInfinity(x) || x = Double.MaxValue ||x = Double.MinValue || x = Double.Epsilon || x = -Double.Epsilon then rnd.NextDouble() else x

    [<Property>]
    let ``sum``(v : float[]) =
        setVector app "v" v
        app.Execute("res = sum(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 0) ==> lazy(epsEqual 1e-15 [|sum(v)|] res))

    [<Property>]
    let ``prod``(v : float[]) =
        setVector app "v" v
        app.Execute("res = prod(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 0) ==> lazy(epsEqual 1e-15 [|prod(v)|] res))

    [<Property>]
    let ``cumsum``(v : float[]) =
        setVector app "v" v
        app.Execute("res = cumsum(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (cumsum(v).ToArray()) res

    [<Property>]
    let ``cumprod``(v : float[]) =
        setVector app "v" v
        app.Execute("res = cumprod(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (cumprod(v).ToArray()) res

    [<Property>]
    let ``min``(v : float[]) =
        setVector app "v" v
        app.Execute("res = min(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 0) ==> lazy(epsEqual 1e-15 [|min(v)|] res))

    [<Property>]
    let ``max``(v : float[]) =
        setVector app "v" v
        app.Execute("res = max(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 0) ==> lazy(epsEqual 1e-15 [|max(v)|] res))

    [<Property>]
    let ``mean``(v : float[]) =
        setVector app "v" v
        app.Execute("res = mean(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 0) ==> lazy(epsEqual 1e-15 [|mean(v)|] res))

    [<Property>]
    let ``var``(v : float[]) =
        setVector app "v" v
        app.Execute("res = var(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 0) ==> lazy(epsEqual 1e-13 [|var(v)|] res || (Double.IsInfinity(res.[0]) && Double.IsNaN(var(v)))))

    [<Property>]
    let ``skewness``(v : float[]) =
        let v = v |> Array.map regDouble
        setVector app "v" v
        app.Execute("res = skewness(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 1) ==> lazy(epsEqual 1e-5 [|skewness(v)|] res))

    [<Property>]
    let ``kurtosis``(v : float[]) =
        let v = v |> Array.map regDouble
        setVector app "v" v
        app.Execute("res = kurtosis(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ((v.Length > 1) ==> lazy(epsEqual 1e-5 [|kurtosis(v)|] res))

    [<Fact>]
    let ``quantile``() =
        use v : Vector = unifRnd rng 0.0 1000.0 10000000L
        let q = new Vector([|1.0..(-0.1)..0.0|])
        let res = [|1000.0..(-100.)..0.0|]
        epsEqual 1e-3 ((quantile v q).ToArray()) res |> should be True

