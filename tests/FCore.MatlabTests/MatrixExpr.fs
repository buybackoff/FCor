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

module MatrixExpr = 

    let inline (<=>) (x : Matrix) (y : Matrix) = epsEqualArray2D (x.ToArray2D()) (y.ToArray2D()) epsEqualFloat 0.0

    [<Property>]
    let ``MatrixExpr .< MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``MatrixExpr .< Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .< Y) = (X .< Y)

    [<Property>]
    let ``Matrix .< MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``MatrixExpr .< scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .< a) = (X .< a))

    [<Property>]
    let ``scalar .< MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .< X.AsExpr) = (a .< X))


    [<Property>]
    let ``MatrixExpr .<= MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``MatrixExpr .<= Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .<= Y) = (X .<= Y)

    [<Property>]
    let ``Matrix .<= MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``MatrixExpr .<= scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<= a) = (X .<= a))

    [<Property>]
    let ``scalar .<= MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .<= X.AsExpr) = (a .<= X))


    [<Property>]
    let ``MatrixExpr .> MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``MatrixExpr .> Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .> Y) = (X .> Y)

    [<Property>]
    let ``Matrix .> MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``MatrixExpr .> scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .> a) = (X .> a))

    [<Property>]
    let ``scalar .> MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .> X.AsExpr) = (a .> X))


    [<Property>]
    let ``MatrixExpr .>= MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``MatrixExpr .>= Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .>= Y) = (X .>= Y)

    [<Property>]
    let ``Matrix .>= MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``MatrixExpr .>= scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .>= a) = (X .>= a))

    [<Property>]
    let ``scalar .>= MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .>= X.AsExpr) = (a .>= X))


    [<Property>]
    let ``MatrixExpr .= MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``MatrixExpr .= Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .= Y) = (X .= Y)

    [<Property>]
    let ``Matrix .= MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``MatrixExpr .= scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .= a) = (X .= a))

    [<Property>]
    let ``scalar .= MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .= X.AsExpr) = (a .= X))


    [<Property>]
    let ``MatrixExpr .<> MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``MatrixExpr .<> Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .<> Y) = (X .<> Y)

    [<Property>]
    let ``Matrix .<> MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``MatrixExpr .<> scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<> a) = (X .<> a))

    [<Property>]
    let ``scalar .<> MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .<> X.AsExpr) = (a .<> X))

    [<Property>]
    let ``Min MatrixExpr MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (MatrixExpr.Min(X.AsExpr, Y.AsExpr)) <=> Matrix.Min(X, Y)

    [<Property>]
    let ``Min MatrixExpr Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (MatrixExpr.Min(X.AsExpr, Y)) <=> Matrix.Min(X, Y)

    [<Property>]
    let ``Min Matrix MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (MatrixExpr.Min(X, Y.AsExpr)) <=> Matrix.Min(X, Y)

    [<Property>]
    let ``Min MatrixExpr scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (MatrixExpr.Min(X.AsExpr, a)) <=> Matrix.Min(X, a))

    [<Property>]
    let ``Min scalar MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (MatrixExpr.Min(a, X.AsExpr)) <=> Matrix.Min(a, X))

    [<Property>]
    let ``Max MatrixExpr MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (MatrixExpr.Max(X.AsExpr, Y.AsExpr)) <=> Matrix.Max(X, Y)

    [<Property>]
    let ``Max MatrixExpr Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (MatrixExpr.Max(X.AsExpr, Y)) <=> Matrix.Max(X, Y)

    [<Property>]
    let ``Max Matrix MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (MatrixExpr.Max(X, Y.AsExpr)) <=> Matrix.Max(X, Y)

    [<Property>]
    let ``Max MatrixExpr scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (MatrixExpr.Max(X.AsExpr, a)) <=> Matrix.Max(X, a))

    [<Property>]
    let ``Max scalar MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (MatrixExpr.Max(a, X.AsExpr)) <=> Matrix.Max(a, X))



    [<Property>]
    let ``MatrixExpr .* MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .* Y.AsExpr) <=> (X .* Y)

    [<Property>]
    let ``MatrixExpr .* Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .* Y) <=> (X .* Y)

    [<Property>]
    let ``Matrix .* MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .* Y.AsExpr) <=> (X .* Y)

    [<Property>]
    let ``MatrixExpr .* scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .* a) <=> (X .* a))

    [<Property>]
    let ``scalar .* MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .* X.AsExpr) <=> (a .* X))


    [<Property>]
    let ``MatrixExpr ./ MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr ./ Y.AsExpr) <=> (X ./ Y)

    [<Property>]
    let ``MatrixExpr ./ Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr ./ Y) <=> (X ./ Y)

    [<Property>]
    let ``Matrix ./ MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X ./ Y.AsExpr) <=> (X ./ Y)

    [<Property>]
    let ``MatrixExpr ./ scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr ./ a) <=> (X ./ a))

    [<Property>]
    let ``scalar ./ MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a ./ X.AsExpr) <=> (a ./ X))

    [<Property>]
    let ``MatrixExpr + MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr + Y.AsExpr) <=> (X + Y)

    [<Property>]
    let ``MatrixExpr + Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr + Y) <=> (X + Y)

    [<Property>]
    let ``Matrix + MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X + Y.AsExpr) <=> (X + Y)

    [<Property>]
    let ``MatrixExpr + scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr + a) <=> (X + a))

    [<Property>]
    let ``scalar + MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a + X.AsExpr) <=> (a + X))

    [<Property>]
    let ``MatrixExpr - MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr - Y.AsExpr) <=> (X - Y)

    [<Property>]
    let ``MatrixExpr - Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr - Y) <=> (X - Y)

    [<Property>]
    let ``Matrix - MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X - Y.AsExpr) <=> (X - Y)

    [<Property>]
    let ``MatrixExpr - scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr - a) <=> (X - a))

    [<Property>]
    let ``scalar - MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a - X.AsExpr) <=> (a - X))

    [<Property>]
    let ``MatrixExpr .^ MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .^ Y.AsExpr) <=> (X .^ Y)

    [<Property>]
    let ``MatrixExpr .^ Matrix`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X.AsExpr .^ Y) <=> (X .^ Y)

    [<Property>]
    let ``Matrix .^ MatrixExpr`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        eval (X .^ Y.AsExpr) <=> (X .^ Y)

    [<Property>]
    let ``MatrixExpr .^ scalar`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .^ a) <=> (X .^ a))

    [<Property>]
    let ``scalar .^ MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .^ X.AsExpr) <=> (a .^ X))

    [<Property>]
    let ``- MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (-X.AsExpr) <=> (-X)

    [<Property>]
    let ``abs MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (abs X.AsExpr) <=> (abs X)

    [<Property>]
    let ``sqrt MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (sqrt X.AsExpr) <=> (sqrt X)

    [<Property>]
    let ``sin MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (sin X.AsExpr) <=> (sin X)

    [<Property>]
    let ``cos MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (cos X.AsExpr) <=> (cos X)

    [<Property>]
    let ``tan MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (tan X.AsExpr) <=> (tan X)

    [<Property>]
    let ``asin MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (asin X.AsExpr) <=> (asin X)

    [<Property>]
    let ``acos MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (acos X.AsExpr) <=> (acos X)

    [<Property>]
    let ``atan MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (atan X.AsExpr) <=> (atan X)

    [<Property>]
    let ``sinh MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (sinh X.AsExpr) <=> (sinh X)

    [<Property>]
    let ``cosh MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (cosh X.AsExpr) <=> (cosh X)

    [<Property>]
    let ``tanh MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (tanh X.AsExpr) <=> (tanh X)

    [<Property>]
    let ``asinh MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (asinh X.AsExpr) <=> (asinh X)

    [<Property>]
    let ``acosh MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (acosh X.AsExpr) <=> (acosh X)

    [<Property>]
    let ``atanh MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (atanh X.AsExpr) <=> (atanh X)

    [<Property>]
    let ``exp MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (exp X.AsExpr) <=> (exp X)

    [<Property>]
    let ``expm1 MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (expm1 X.AsExpr) <=> (expm1 X)

    [<Property>]
    let ``log MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (log X.AsExpr) <=> (log X)

    [<Property>]
    let ``log10 MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (log10 X.AsExpr) <=> (log10 X)

    [<Property>]
    let ``log1p MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (log1p X.AsExpr) <=> (log1p X)

    [<Property>]
    let ``erf MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (erf X.AsExpr) <=> (erf X)

    [<Property>]
    let ``erfc MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (erfc X.AsExpr) <=> (erfc X)

    [<Property>]
    let ``erfinv MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (erfinv X.AsExpr) <=> (erfinv X)

    [<Property>]
    let ``erfcinv MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (erfcinv X.AsExpr) <=> (erfcinv X)

    [<Property>]
    let ``normcdf MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (normcdf X.AsExpr) <=> (normcdf X)

    [<Property>]
    let ``norminv MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (norminv X.AsExpr) <=> (norminv X)

    [<Property>]
    let ``round MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (round X.AsExpr) <=> (round X)

    [<Property>]
    let ``ceil MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (ceil X.AsExpr) <=> (ceil X)

    [<Property>]
    let ``floor MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (floor X.AsExpr) <=> (floor X)

    [<Property>]
    let ``trunc MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (trunc X.AsExpr) <=> (trunc X)

