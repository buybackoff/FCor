namespace FCore.MatlabTests

open FCore
open FCore.Math
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module VectorFunctions =

    let app = new MLAppClass()
    do app.Visible <- 0

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[]) (y :float[])  = epsEqualArray x y epsEqualFloat eps

    [<Property>]
    let ``Abs``(v : float[]) =
        setVector app "v" v
        app.Execute("res = abs(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        abs(v).ToArray() <=> res

    [<Property>]
    let ``Sqrt``(v : float[]) =
        let v = v |> Array.map Math.Abs
        setVector app "v" v
        app.Execute("res = sqrt(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (sqrt(v).ToArray()) res

    [<Property>]
    let ``Sin``(v : float[]) =
        setVector app "v" v
        app.Execute("res = sin(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (sin(v).ToArray()) res

    [<Property>]
    let ``Cos``(v : float[]) =
        setVector app "v" v
        app.Execute("res = cos(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (cos(v).ToArray()) res

    [<Property>]
    let ``Tan``(v : float[]) =
        setVector app "v" v
        app.Execute("res = tan(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (tan(v).ToArray()) res

    [<Property>]
    let ``ASin``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= -1. && x <= 1.)
        setVector app "v" v
        app.Execute("res = asin(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (asin(v).ToArray()) res

    [<Property>]
    let ``ACos``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= -1. && x <= 1.)
        setVector app "v" v
        app.Execute("res = acos(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (acos(v).ToArray()) res

    [<Property>]
    let ``ATan``(v : float[]) =
        setVector app "v" v
        app.Execute("res = atan(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (atan(v).ToArray()) res

    [<Property>]
    let ``Sinh``(v : float[]) =
        setVector app "v" v
        app.Execute("res = sinh(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (sinh(v).ToArray()) res

    [<Property>]
    let ``Cosh``(v : float[]) =
        setVector app "v" v
        app.Execute("res = cosh(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (cosh(v).ToArray()) res

    [<Property>]
    let ``Tanh``(v : float[]) =
        setVector app "v" v
        app.Execute("res = tanh(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (tanh(v).ToArray()) res

    [<Property>]
    let ``ASinh``(v : float[]) =
        setVector app "v" v
        app.Execute("res = asinh(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (asinh(v).ToArray()) res

    [<Property>]
    let ``ACosh``(v : float[]) =
        let v = v |> Array.filter (fun x -> x > 1.0)
        setVector app "v" v
        app.Execute("res = acosh(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (acosh(v).ToArray()) res

    [<Property>]
    let ``ATanh``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= -1. && x <= 1.)
        setVector app "v" v
        app.Execute("res = atanh(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (atanh(v).ToArray()) res

    [<Property>]
    let ``Exp``(v : float[]) =
        setVector app "v" v
        app.Execute("res = exp(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (exp(v).ToArray()) res

    [<Property>]
    let ``Expm1``(v : float[]) =
        setVector app "v" v
        app.Execute("res = expm1(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (expm1(v).ToArray()) res

    [<Property>]
    let ``Log``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= 0.0)
        setVector app "v" v
        app.Execute("res = log(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (log(v).ToArray()) res

    [<Property>]
    let ``Log10``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= 0.0)
        setVector app "v" v
        app.Execute("res = log10(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (log10(v).ToArray()) res

    [<Property>]
    let ``Log1p``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= -1.)
        setVector app "v" v
        app.Execute("res = log1p(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (log1p(v).ToArray()) res

    [<Property>]
    let ``erf``(v : float[]) =
        setVector app "v" v
        app.Execute("res = erf(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (erf(v).ToArray()) res

    [<Property>]
    let ``erfc``(v : float[]) =
        setVector app "v" v
        app.Execute("res = erfc(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (erfc(v).ToArray()) res

    [<Property>]
    let ``erfinv``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= -1. && x <= 1.0)
        setVector app "v" v
        app.Execute("res = erfinv(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (erfinv(v).ToArray()) res

    [<Property>]
    let ``erfcinv``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= 0.0 && x <= 2.0) |> Array.map (fun x -> if x = Double.Epsilon then 0. else x)
        setVector app "v" v
        app.Execute("res = erfcinv(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (erfcinv(v).ToArray()) res

    [<Property>]
    let ``normcdf``(v : float[]) =
        let v = v |> Array.map  (fun x -> if x < -5.0 then -5.0 else x)
        setVector app "v" v
        app.Execute("res = normcdf(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-13 (normcdf(v).ToArray()) res

    [<Property>]
    let ``norminv``(v : float[]) =
        let v = v |> Array.filter (fun x -> x >= 0.0 && x <= 1.0) |> Array.map (fun x -> if x = Double.Epsilon then 0. else x)
        setVector app "v" v
        app.Execute("res = norminv(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        epsEqual 1e-15 (norminv(v).ToArray()) res

    [<Property>]
    let ``round``(v : float[]) =
        setVector app "v" v
        app.Execute("res = round(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        round(v).ToArray() <=> res

    [<Property>]
    let ``ceil``(v : float[]) =
        setVector app "v" v
        app.Execute("res = ceil(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        ceil(v).ToArray() <=> res

    [<Property>]
    let ``floor``(v : float[]) =
        setVector app "v" v
        app.Execute("res = floor(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        floor(v).ToArray() <=> res

    [<Property>]
    let ``trunc``(v : float[]) =
        setVector app "v" v
        app.Execute("res = fix(v);") |> ignore
        let res = getVector app "res"
        let v = new Vector(v)
        trunc(v).ToArray() <=> res


