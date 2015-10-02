namespace FCore.MatlabTests

open FCore
open FCore.Math
open FCore.ExplicitConversion
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open Util

module VectorExpr = 

    let rnd = new Random()

    let inline (<=>) (x : Vector) (y : Vector) = epsEqualArray (x.ToArray()) (y.ToArray()) epsEqualFloat 0.0

    let inline (<==>) (x : float) (y : float) = epsEqualFloat x y 0.0

    let inline epsEqual eps (x : Vector) (y : Vector) = epsEqualArray (x.ToArray()) (y.ToArray()) epsEqualFloat eps

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
    let ``VectorExpr .^ n`` (x : float[]) (n : int) =
        let n = max (min 10 n) -10
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(epsEqual 1e-14 (eval (X.AsExpr .^ n)) (X .^ n))

    [<Property>]
    let ``scalar .^ VectorExpr`` (x : float[]) (a : float) =
        let X = new Vector(x)
        (x.Length > 0) ==> lazy(eval (a .^ X.AsExpr) <=> (a .^ X))

    [<Property>]
    let ``- VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        eval (-X.AsExpr) <=> (-X)

    [<Property>]
    let ``-(-VectorExpr)`` (x : float[]) =
        let X = new Vector(x)
        eval (-(-X.AsExpr)) <=> X

    [<Property>]
    let ``a .* X + b .* Y`` (v : (float*float)[]) (a : float) (b : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (a .* X.AsExpr + b .* Y.AsExpr) <=> (a .* X + b .* Y)

    [<Property>]
    let ``X .* a + b .* Y`` (v : (float*float)[]) (a : float) (b : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .* a + b .* Y.AsExpr) <=> (a .* X + b .* Y)

    [<Property>]
    let ``a .* X + Y .* b`` (v : (float*float)[]) (a : float) (b : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (a .* X.AsExpr + Y.AsExpr .* b) <=> (a .* X + b .* Y)

    [<Property>]
    let ``X .* a + Y .* b`` (v : (float*float)[]) (a : float) (b : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        eval (X.AsExpr .* a + Y.AsExpr .* b) <=> (a .* X + b .* Y)

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

    [<Property>]
    let ``EvalIn -VectorExpr`` (x : float[]) =
        let X = new Vector(x)
        let res = -X
        let evalRes = evalIn X (-X.AsExpr)
        X <=> res

    [<Fact>]
    let ``EvalIn(expr, None) throws arg exn if expr has no matching elementwise length``() =
        let x = new Vector(6,0.0)
        let y = new Vector(5,1.0)
        Assert.Throws<ArgumentException>(fun () -> VectorExpr.EvalIn(x.AsExpr .* y, None) |> ignore)

    [<Fact>]
    let ``EvalIn(expr, Some v) throws arg exn if v has no matching elementwise length``() =
        let x = new Vector(6,0.0)
        let y = new Vector(6,1.0)
        let res = new Vector(5, 0.0)
        Assert.Throws<ArgumentException>(fun () -> VectorExpr.EvalIn(x.AsExpr .* y, Some res) |> ignore) 

    [<Property>]
    let ``Eval scalar Var`` (a : float) =
        let x = new Vector(a)
        eval x.AsExpr <=> x

    [<Property>]
    let ``Eval scalar UnaryFunction`` (a : float) =
        let x = new Vector(a)
        eval (x.AsExpr |> (~-) |> (~-) |> (~-)) <=> new Vector(a |> (~-) |> (~-) |> (~-))

    [<Property>]
    let ``Eval scalar BinaryFunction`` (a1 : float) (a2 : float) (b1 : float) (b2 : float) =
        let x1 = new Vector(a1)
        let x2 = new Vector(a2)
        let y1 = new Vector(b1)
        let y2 = new Vector(b2)
        let r = (a1 * a2) + (b1 * b2)
        (not <| Double.IsNaN(r) && not <| Double.IsInfinity(r)) ==> ( epsEqual 1e-15 (eval ((x1.AsExpr .* x2) + (y1.AsExpr .* y2))) (new Vector( (a1 * a2) + (b1 * b2))))

    [<Property>]
    let ``Eval scalar IfFunction`` (a : bool) (b : float) (c : float) =
        let x = new BoolVector(a)
        let y = new Vector(b)
        let z = new Vector(c)
        eval (iif (BoolVectorExpr.Not(x.AsExpr)) (-y.AsExpr) (-z.AsExpr)) <=> new Vector(if not a then -b else -c)

    [<Property>]
    let ``eval (scalar .* Vector) + Vector`` (v : (float*float)[]) (a : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (v.Length > 0) ==> lazy(eval ((a .* X.AsExpr) + Y) <=> ((a .* X) + Y))

    [<Property>]
    let ``eval (Vector * (scalar + Vector)`` (v : (float*float)[]) (a : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (v.Length > 0) ==> lazy(eval (X .* (a + Y.AsExpr)) <=> (X .* (a + Y)))

    [<Property>]
    let ``eval iif (a .&& X) Y b`` (v : (bool*float)[]) (a : bool) (b : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new Vector(y)
        let aX = a .&& X
        (v.Length > 0) ==> lazy(let res = eval (iif (a .&& X.AsExpr) Y !!b) in res.ToArray() |> Array.mapi (fun i x -> res.[i] <==> if aX.[i] then Y.[i] else b) |> Array.fold (&&) true)

    [<Property>]
    let ``eval iif X a (b .* Y)`` (v : (bool*float)[]) (a : float) (b : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new Vector(y)
        let bY = b .* Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr !!a (b .* Y)) in res.ToArray() |> Array.mapi (fun i x -> res.[i] <==> if X.[i] then a else bY.[i]) |> Array.fold (&&) true)


    [<Property>]
    let ``eval iif X (a .* Y) b`` (v : (bool*float)[]) (a : float) (b : float) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new Vector(y)
        let aY = a .* Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr (a .* Y) !!b) in res.ToArray() |> Array.mapi (fun i x -> res.[i] <==> if X.[i] then aY.[i] else b) |> Array.fold (&&) true)

    [<Property>]
    let ``eval large vector`` (n : int) =
        let n = max (min n 1000000) 0
        use x = new Vector(n, fun i -> rnd.NextDouble())
        use y = new Vector(n, fun i -> rnd.NextDouble())
        eval (VectorExpr.Min(x.AsExpr .* y.AsExpr, x.AsExpr + y.AsExpr) |> (~-)) <=> -Vector.Min(x .* y, x + y)




































































