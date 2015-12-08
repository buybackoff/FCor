namespace FCor.Matrix.Tests

open FCor
open FCor.Math
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module MatrixFunctions =
    
    let rnd = new Random()

    let inline (<=>) (x : float[,]) (y :float[,]) = epsEqualArray2D x y epsEqualFloat 0.0

    let inline (<==>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[,]) (y :float[,])  = epsEqualArray2D x y epsEqualFloat eps

    [<Property>]
    let ``Abs``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = abs(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        abs(v).ToArray2D() <=> res

    [<Property>]
    let ``Sqrt``(v : float[,]) =
        let v = v |> Array2D.map Math.Abs
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = sqrt(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (sqrt(v).ToArray2D()) res

    [<Property>]
    let ``Sin``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = sin(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (sin(v).ToArray2D()) res

    [<Property>]
    let ``Cos``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = cos(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (cos(v).ToArray2D()) res

    [<Property>]
    let ``Tan``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = tan(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (tan(v).ToArray2D()) res

    [<Property>]
    let ``ASin``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= -1. && x <= 1.) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = asin(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (asin(v).ToArray2D()) res

    [<Property>]
    let ``ACos``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= -1. && x <= 1.) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = acos(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (acos(v).ToArray2D()) res

    [<Property>]
    let ``ATan``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = atan(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (atan(v).ToArray2D()) res

    [<Property>]
    let ``Sinh``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = sinh(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (sinh(v).ToArray2D()) res

    [<Property>]
    let ``Cosh``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = cosh(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (cosh(v).ToArray2D()) res

    [<Property>]
    let ``Tanh``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = tanh(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (tanh(v).ToArray2D()) res

    [<Property>]
    let ``ASinh``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = asinh(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (asinh(v).ToArray2D()) res

    [<Property>]
    let ``ACosh``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x > 1.) then 2.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = acosh(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (acosh(v).ToArray2D()) res

    [<Property>]
    let ``ATanh``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= -1. && x <= 1.) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = atanh(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (atanh(v).ToArray2D()) res

    [<Property>]
    let ``Exp``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = exp(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (exp(v).ToArray2D()) res

    [<Property>]
    let ``Expm1``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = expm1(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (expm1(v).ToArray2D()) res

    [<Property>]
    let ``Log``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= 0.0) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = log(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (log(v).ToArray2D()) res

    [<Property>]
    let ``Log10``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= 0.0) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = log10(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (log10(v).ToArray2D()) res

    [<Property>]
    let ``Log1p``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= -1.) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = log1p(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (log1p(v).ToArray2D()) res

    [<Property>]
    let ``erf``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = erf(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (erf(v).ToArray2D()) res

    [<Property>]
    let ``erfc``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = erfc(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (erfc(v).ToArray2D()) res

    [<Property>]
    let ``erfinv``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= -1. && x <= 1.) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = erfinv(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (erfinv(v).ToArray2D()) res

    [<Property>]
    let ``erfcinv``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= 0. && x <= 2.) || (x = Double.Epsilon) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = erfcinv(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (erfcinv(v).ToArray2D()) res

    [<Property>]
    let ``normcdf``(v : float[,]) =
        let v = v |> Array2D.map  (fun x -> if x < -5.0 then -5.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = normcdf(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-13 (normcdf(v).ToArray2D()) res

    [<Property>]
    let ``norminv``(v : float[,]) =
        let v = v |> Array2D.map (fun x -> if not (x >= 0. && x <= 1.) || (x = Double.Epsilon) then 0.0 else x)
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = norminv(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        epsEqual 1e-15 (norminv(v).ToArray2D()) res

    [<Property>]
    let ``round``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = round(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        round(v).ToArray2D() <=> res

    [<Property>]
    let ``ceil``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = ceil(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        ceil(v).ToArray2D() <=> res

    [<Property>]
    let ``floor``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = floor(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        floor(v).ToArray2D() <=> res

    [<Property>]
    let ``trunc``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        app.Execute("res = fix(v);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        trunc(v).ToArray2D() <=> res

    [<Property>]
    let ``triL int64``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        let offset = rnd.Next(max (v.GetLength(0)) (v.GetLength(1))) + 2
        setScalar app "offset" (float(offset))
        app.Execute("res = tril(v,offset);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        (triL v (int64(offset))).ToArray2D() <=> res

    [<Property>]
    let ``triL int``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        let offset = rnd.Next(max (v.GetLength(0)) (v.GetLength(1))) + 2
        setScalar app "offset" (float(offset))
        app.Execute("res = tril(v,offset);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        (triL v offset).ToArray2D() <=> res

    [<Property>]
    let ``triU int64``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        let offset = rnd.Next(max (v.GetLength(0)) (v.GetLength(1))) + 2
        setScalar app "offset" (float(offset))
        app.Execute("res = triu(v,offset);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        (triU v (int64(offset))).ToArray2D() <=> res

    [<Property>]
    let ``triU int``(v : float[,]) =
        let app = getApp()
        setMatrix app "v" v
        let offset = rnd.Next(max (v.GetLength(0)) (v.GetLength(1))) + 2
        setScalar app "offset" (float(offset))
        app.Execute("res = triu(v,offset);") |> ignore
        let res = getMatrix app "res"
        let v = new Matrix(v)
        (triU v offset).ToArray2D() <=> res

    [<Property>]
    let ``diag int64``(v : float[,]) =
        (v.GetLength(0) > 1 && v.GetLength(1) > 1) ==>
            lazy(
                let app = getApp()
                let v = v |> fixEmpty
                setMatrix app "v" v
                let offset = rnd.Next(max (v.GetLength(0)) (v.GetLength(1))) + 2
                setScalar app "offset" (float(offset))
                app.Execute("res = diag(v,offset);") |> ignore
                let res = getVector app "res"
                let v = new Matrix(v)
                (diag v (int64(offset))).ToArray() <==> res            
                )


    [<Property>]
    let ``diag int``(v : float[,]) =
        (v.GetLength(0) > 1 && v.GetLength(1) > 1) ==>
            lazy(
                let app = getApp()
                let v = v |> fixEmpty
                setMatrix app "v" v
                let offset = rnd.Next(max (v.GetLength(0)) (v.GetLength(1))) + 2
                setScalar app "offset" (float(offset))
                app.Execute("res = diag(v,offset);") |> ignore
                let res = getVector app "res"
                let v = new Matrix(v)
                (diag v offset).ToArray() <==> res            
                )

