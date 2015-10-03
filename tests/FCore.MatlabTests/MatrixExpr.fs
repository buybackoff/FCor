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

module MatrixExpr = 

    let rnd = new Random()

    let inline (<=>) (x : Matrix) (y : Matrix) = epsEqualArray2D (x.ToArray2D()) (y.ToArray2D()) epsEqualFloat 0.0
    let inline (<==>) (x : float) (y : float) = epsEqualFloat x y 0.0
    let inline epsEqual eps (x : Matrix) (y : Matrix) = epsEqualArray2D (x.ToArray2D()) (y.ToArray2D()) epsEqualFloat eps
    let isRegular x =
        not <| Double.IsNaN(x) && not <| Double.IsInfinity(x) && x <> Double.MaxValue && x <> Double.MinValue
    
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
    let ``MatrixExpr .^ n`` (x : float[,]) (n : int) =
        let n = max (min 10 n) -10
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(epsEqual 1e-14 (eval (X.AsExpr .^ n)) (X .^ n))

    [<Property>]
    let ``scalar .^ MatrixExpr`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        (x.Length > 0) ==> lazy(eval (a .^ X.AsExpr) <=> (a .^ X))

    [<Property>]
    let ``- MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        eval (-X.AsExpr) <=> (-X)

    [<Property>]
    let ``-(-MatrixExpr)`` (x : float[,]) =
        let X = new Matrix(x)
        eval (-(-X.AsExpr)) <=> X

    [<Property>]
    let ``a .* X + b .* Y`` (v : (float*float)[,]) (a : float) (b : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let a = if a = 0.0 then 1.0 else a
        let b = if b = 0.0 then 1.0 else b
        let X = new Matrix(x)
        let Y = new Matrix(y)
        epsEqual 1e-14 (eval (a .* X.AsExpr + b .* Y.AsExpr)) (a .* X + b .* Y)

    [<Property>]
    let ``X .* a + b .* Y`` (v : (float*float)[,]) (a : float) (b : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let a = if a = 0.0 then 1.0 else a
        let b = if b = 0.0 then 1.0 else b
        let X = new Matrix(x)
        let Y = new Matrix(y)
        epsEqual 1e-14 (eval (X.AsExpr .* a + b .* Y.AsExpr)) (a .* X + b .* Y)

    [<Property>]
    let ``a .* X + Y .* b`` (v : (float*float)[,]) (a : float) (b : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let a = if a = 0.0 then 1.0 else a
        let b = if b = 0.0 then 1.0 else b
        let X = new Matrix(x)
        let Y = new Matrix(y)
        epsEqual 1e-14 (eval (a .* X.AsExpr + Y.AsExpr .* b)) (a .* X + b .* Y)

    [<Property>]
    let ``X .* a + Y .* b`` (v : (float*float)[,]) (a : float) (b : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let a = if a = 0.0 then 1.0 else a
        let b = if b = 0.0 then 1.0 else b
        let X = new Matrix(x)
        let Y = new Matrix(y)
        epsEqual 1e-14 (eval (X.AsExpr .* a + Y.AsExpr .* b)) (a .* X + b .* Y)

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

    [<Property>]
    let ``EvalIn -MatrixExpr`` (x : float[,]) =
        let X = new Matrix(x)
        let res = -X
        let evalRes = evalIn X (-X.AsExpr)
        X <=> res

    [<Fact>]
    let ``EvalIn(expr, None) throws arg exn if expr has no matching elementwise size``() =
        let x = new Matrix(2,3,0.0)
        let y = new Matrix(3,2,1.0)
        Assert.Throws<ArgumentException>(fun () -> MatrixExpr.EvalIn(x.AsExpr .* y, None) |> ignore)

    [<Fact>]
    let ``EvalIn(expr, Some v) throws arg exn if v has no matching elementwise size``() =
        let x = new Matrix(2,3,0.0)
        let y = new Matrix(2,3,1.0)
        let res = new Matrix(3,2, 0.0)
        Assert.Throws<ArgumentException>(fun () -> MatrixExpr.EvalIn(x.AsExpr .* y, Some res) |> ignore) 

    [<Property>]
    let ``Eval scalar Var`` (a : float) =
        let x = new Matrix(a)
        eval x.AsExpr <=> x

    [<Property>]
    let ``Eval scalar UnaryFunction`` (a : float) =
        let x = new Matrix(a)
        eval (x.AsExpr |> (~-) |> (~-) |> (~-)) <=> new Matrix(a |> (~-) |> (~-) |> (~-))

    [<Property>]
    let ``Eval scalar BinaryFunction`` (a1 : float) (a2 : float) (b1 : float) (b2 : float) =
        let x1 = new Matrix(a1)
        let x2 = new Matrix(a2)
        let y1 = new Matrix(b1)
        let y2 = new Matrix(b2)
        let r = (a1 * a2) + (b1 * b2)
        (isRegular a1 && isRegular a2 && isRegular b1 && isRegular b2) ==> (epsEqual 1e-14 (eval ((x1.AsExpr .* x2) + (y1.AsExpr .* y2))) (new Matrix( (a1 * a2) + (b1 * b2))))

    [<Property>]
    let ``Eval scalar IfFunction`` (a : bool) (b : float) (c : float) =
        let x = new BoolMatrix(a)
        let y = new Matrix(b)
        let z = new Matrix(c)
        eval (iif (BoolMatrixExpr.Not(x.AsExpr)) (-y.AsExpr) (-z.AsExpr)) <=> new Matrix(if not a then -b else -c)

    [<Property>]
    let ``eval (scalar .* Matrix) + Matrix`` (v : (float*float)[,]) (a : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (v.Length > 0) ==> lazy(eval ((a .* X.AsExpr) + Y) <=> ((a .* X) + Y))

    [<Property>]
    let ``eval (Matrix .* (scalar + Matrix)`` (v : (float*float)[,]) (a : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (v.Length > 0) ==> lazy(eval (X .* (a + Y.AsExpr)) <=> (X .* (a + Y)))

    [<Property>]
    let ``eval iif (a && X) Y b`` (v : (bool*float)[,]) (a : bool) (b : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new Matrix(y)
        let aX = a .&& X
        (v.Length > 0) ==> lazy(let res = eval (iif (a .&& X.AsExpr) Y !!b) in res.ToArray2D() |> Array2D.mapi (fun i j x -> res.[i,j] <==> if aX.[i,j] then Y.[i,j] else b) = Array2D.create res.RowCount res.ColCount true)

    [<Property>]
    let ``eval iif X a (b .* Y)`` (v : (bool*float)[,]) (a : float) (b : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new Matrix(y)
        let bY = b .* Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr !!a (b .* Y)) in res.ToArray2D() |> Array2D.mapi (fun i j x -> res.[i,j] <==> if X.[i,j] then a else bY.[i,j]) = Array2D.create res.RowCount res.ColCount true)


    [<Property>]
    let ``eval iif X (a .* Y) b`` (v : (bool*float)[,]) (a : float) (b : float) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new Matrix(y)
        let aY = a .* Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr (a .* Y) !!b) in res.ToArray2D() |> Array2D.mapi (fun i j x -> res.[i,j] <==> if X.[i,j] then aY.[i,j] else b) = Array2D.create res.RowCount res.ColCount true)


    [<Property>]
    let ``eval large vector`` (n: int) =
        let n = max (min n 200000) 30000
        use x = new Matrix(n, 5, fun i j -> rnd.NextDouble())
        use y = new Matrix(n, 5, fun i j -> rnd.NextDouble())
        eval (MatrixExpr.Min(x.AsExpr .* y.AsExpr, x.AsExpr + y.AsExpr) |> (~-)) <=> -Matrix.Min(x .* y, x + y)

