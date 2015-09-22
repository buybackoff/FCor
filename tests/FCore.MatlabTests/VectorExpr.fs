namespace FCore.MatlabTests

open FCore
open FCore.Math
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open Util

module VectorExpr = 

    let inline (<=>) (x : Vector) (y : Vector) = epsEqualArray (x.ToArray()) (y.ToArray()) epsEqualFloat 0.0

    [<Property>]
    let ``VectorExpr .< VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``VectorExpr .< Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .< Y) = (X .< Y)

    [<Property>]
    let ``Vector .< VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``VectorExpr .< scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .< a) = (X .< a))

    [<Property>]
    let ``scalar .< VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .< X.AsExpr) = (a .< X))


    [<Property>]
    let ``VectorExpr .<= VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``VectorExpr .<= Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .<= Y) = (X .<= Y)

    [<Property>]
    let ``Vector .<= VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``VectorExpr .<= scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<= a) = (X .<= a))

    [<Property>]
    let ``scalar .<= VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .<= X.AsExpr) = (a .<= X))


    [<Property>]
    let ``VectorExpr .> VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``VectorExpr .> Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .> Y) = (X .> Y)

    [<Property>]
    let ``Vector .> VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``VectorExpr .> scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .> a) = (X .> a))

    [<Property>]
    let ``scalar .> VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .> X.AsExpr) = (a .> X))


    [<Property>]
    let ``VectorExpr .>= VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``VectorExpr .>= Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .>= Y) = (X .>= Y)

    [<Property>]
    let ``Vector .>= VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``VectorExpr .>= scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .>= a) = (X .>= a))

    [<Property>]
    let ``scalar .>= VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .>= X.AsExpr) = (a .>= X))


    [<Property>]
    let ``VectorExpr .= VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``VectorExpr .= Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .= Y) = (X .= Y)

    [<Property>]
    let ``Vector .= VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``VectorExpr .= scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .= a) = (X .= a))

    [<Property>]
    let ``scalar .= VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .= X.AsExpr) = (a .= X))


    [<Property>]
    let ``VectorExpr .<> VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``VectorExpr .<> Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .<> Y) = (X .<> Y)

    [<Property>]
    let ``Vector .<> VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``VectorExpr .<> scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<> a) = (X .<> a))

    [<Property>]
    let ``scalar .<> VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .<> X.AsExpr) = (a .<> X))

    [<Property>]
    let ``Min VectorExpr VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (VectorExpr.Min(X.AsExpr, Y.AsExpr)) <=> Vector.Min(X, Y)

    [<Property>]
    let ``Min VectorExpr Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (VectorExpr.Min(X.AsExpr, Y)) <=> Vector.Min(X, Y)

    [<Property>]
    let ``Min Vector VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (VectorExpr.Min(X, Y.AsExpr)) <=> Vector.Min(X, Y)

    [<Property>]
    let ``Min VectorExpr scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (VectorExpr.Min(X.AsExpr, a)) <=> Vector.Min(X, a))

    [<Property>]
    let ``Min scalar VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (VectorExpr.Min(a, X.AsExpr)) <=> Vector.Min(a, X))

    [<Property>]
    let ``Max VectorExpr VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (VectorExpr.Max(X.AsExpr, Y.AsExpr)) <=> Vector.Max(X, Y)

    [<Property>]
    let ``Max VectorExpr Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (VectorExpr.Max(X.AsExpr, Y)) <=> Vector.Max(X, Y)

    [<Property>]
    let ``Max Vector VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (VectorExpr.Max(X, Y.AsExpr)) <=> Vector.Max(X, Y)

    [<Property>]
    let ``Max VectorExpr scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (VectorExpr.Max(X.AsExpr, a)) <=> Vector.Max(X, a))

    [<Property>]
    let ``Max scalar VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (VectorExpr.Max(a, X.AsExpr)) <=> Vector.Max(a, X))



    [<Property>]
    let ``VectorExpr .* VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .* Y.AsExpr) <=> (X .* Y)

    [<Property>]
    let ``VectorExpr .* Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .* Y) <=> (X .* Y)

    [<Property>]
    let ``Vector .* VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .* Y.AsExpr) <=> (X .* Y)

    [<Property>]
    let ``VectorExpr .* scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .* a) <=> (X .* a))

    [<Property>]
    let ``scalar .* VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .* X.AsExpr) <=> (a .* X))


    [<Property>]
    let ``VectorExpr ./ VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr ./ Y.AsExpr) <=> (X ./ Y)

    [<Property>]
    let ``VectorExpr ./ Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr ./ Y) <=> (X ./ Y)

    [<Property>]
    let ``Vector ./ VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X ./ Y.AsExpr) <=> (X ./ Y)

    [<Property>]
    let ``VectorExpr ./ scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr ./ a) <=> (X ./ a))

    [<Property>]
    let ``scalar ./ VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a ./ X.AsExpr) <=> (a ./ X))

    [<Property>]
    let ``VectorExpr + VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr + Y.AsExpr) <=> (X + Y)

    [<Property>]
    let ``VectorExpr + Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr + Y) <=> (X + Y)

    [<Property>]
    let ``Vector + VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X + Y.AsExpr) <=> (X + Y)

    [<Property>]
    let ``VectorExpr + scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr + a) <=> (X + a))

    [<Property>]
    let ``scalar + VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a + X.AsExpr) <=> (a + X))

    [<Property>]
    let ``VectorExpr - VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr - Y.AsExpr) <=> (X - Y)

    [<Property>]
    let ``VectorExpr - Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr - Y) <=> (X - Y)

    [<Property>]
    let ``Vector - VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X - Y.AsExpr) <=> (X - Y)

    [<Property>]
    let ``VectorExpr - scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr - a) <=> (X - a))

    [<Property>]
    let ``scalar - VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a - X.AsExpr) <=> (a - X))

    [<Property>]
    let ``VectorExpr .^ VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .^ Y.AsExpr) <=> (X .^ Y)

    [<Property>]
    let ``VectorExpr .^ Vector`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .^ Y) <=> (X .^ Y)

    [<Property>]
    let ``Vector .^ VectorExpr`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X .^ Y.AsExpr) <=> (X .^ Y)

    [<Property>]
    let ``VectorExpr .^ scalar`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .^ a) <=> (X .^ a))

    [<Property>]
    let ``scalar .^ VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .^ X.AsExpr) <=> (a .^ X))

    [<Property>]
    let ``- VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (-X.AsExpr) <=> (-X)

    [<Property>]
    let ``abs VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (abs X.AsExpr) <=> (abs X)

    [<Property>]
    let ``sqrt VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (sqrt X.AsExpr) <=> (sqrt X)

    [<Property>]
    let ``sin VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (sin X.AsExpr) <=> (sin X)

    [<Property>]
    let ``cos VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (cos X.AsExpr) <=> (cos X)

    [<Property>]
    let ``tan VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (tan X.AsExpr) <=> (tan X)

    [<Property>]
    let ``asin VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (asin X.AsExpr) <=> (asin X)

    [<Property>]
    let ``acos VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (acos X.AsExpr) <=> (acos X)

    [<Property>]
    let ``atan VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (atan X.AsExpr) <=> (atan X)

    [<Property>]
    let ``sinh VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (sinh X.AsExpr) <=> (sinh X)

    [<Property>]
    let ``cosh VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (cosh X.AsExpr) <=> (cosh X)

    [<Property>]
    let ``tanh VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (tanh X.AsExpr) <=> (tanh X)

    [<Property>]
    let ``asinh VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (asinh X.AsExpr) <=> (asinh X)

    [<Property>]
    let ``acosh VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (acosh X.AsExpr) <=> (acosh X)

    [<Property>]
    let ``atanh VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (atanh X.AsExpr) <=> (atanh X)

    [<Property>]
    let ``exp VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (exp X.AsExpr) <=> (exp X)

    [<Property>]
    let ``expm1 VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (expm1 X.AsExpr) <=> (expm1 X)

    [<Property>]
    let ``log VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (log X.AsExpr) <=> (log X)

    [<Property>]
    let ``log10 VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (log10 X.AsExpr) <=> (log10 X)

    [<Property>]
    let ``log1p VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (log1p X.AsExpr) <=> (log1p X)

    [<Property>]
    let ``erf VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (erf X.AsExpr) <=> (erf X)

    [<Property>]
    let ``erfc VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (erfc X.AsExpr) <=> (erfc X)

    [<Property>]
    let ``erfinv VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (erfinv X.AsExpr) <=> (erfinv X)

    [<Property>]
    let ``erfcinv VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (erfcinv X.AsExpr) <=> (erfcinv X)

    [<Property>]
    let ``normcdf VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (normcdf X.AsExpr) <=> (normcdf X)

    [<Property>]
    let ``norminv VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (norminv X.AsExpr) <=> (norminv X)

    [<Property>]
    let ``round VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (round X.AsExpr) <=> (round X)

    [<Property>]
    let ``ceil VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (ceil X.AsExpr) <=> (ceil X)

    [<Property>]
    let ``floor VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (floor X.AsExpr) <=> (floor X)

    [<Property>]
    let ``trunc VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (trunc X.AsExpr) <=> (trunc X)


































































