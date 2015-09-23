namespace FCore.MatlabTests

open FCore
open FCore.Math
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolMatrixExpr = 

    [<Property>]
    let ``BoolMatrixExpr .< BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``BoolMatrixExpr .< BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .< Y) = (X .< Y)

    [<Property>]
    let ``BoolMatrix .< BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``BoolMatrixExpr .< scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .< a) = (X .< a))

    [<Property>]
    let ``scalar .< BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .< X.AsExpr) = (a .< X))


    [<Property>]
    let ``BoolMatrixExpr .<= BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``BoolMatrixExpr .<= BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .<= Y) = (X .<= Y)

    [<Property>]
    let ``BoolMatrix .<= BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``BoolMatrixExpr .<= scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<= a) = (X .<= a))

    [<Property>]
    let ``scalar .<= BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .<= X.AsExpr) = (a .<= X))


    [<Property>]
    let ``BoolMatrixExpr .> BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``BoolMatrixExpr .> BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .> Y) = (X .> Y)

    [<Property>]
    let ``BoolMatrix .> BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``BoolMatrixExpr .> scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .> a) = (X .> a))

    [<Property>]
    let ``scalar .> BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .> X.AsExpr) = (a .> X))


    [<Property>]
    let ``BoolMatrixExpr .>= BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``BoolMatrixExpr .>= BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .>= Y) = (X .>= Y)

    [<Property>]
    let ``BoolMatrix .>= BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``BoolMatrixExpr .>= scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .>= a) = (X .>= a))

    [<Property>]
    let ``scalar .>= BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .>= X.AsExpr) = (a .>= X))


    [<Property>]
    let ``BoolMatrixExpr .= BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``BoolMatrixExpr .= BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .= Y) = (X .= Y)

    [<Property>]
    let ``BoolMatrix .= BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``BoolMatrixExpr .= scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .= a) = (X .= a))

    [<Property>]
    let ``scalar .= BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .= X.AsExpr) = (a .= X))


    [<Property>]
    let ``BoolMatrixExpr .<> BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``BoolMatrixExpr .<> BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .<> Y) = (X .<> Y)

    [<Property>]
    let ``BoolMatrix .<> BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``BoolMatrixExpr .<> scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<> a) = (X .<> a))

    [<Property>]
    let ``scalar .<> BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .<> X.AsExpr) = (a .<> X))

    [<Property>]
    let ``Min BoolMatrixExpr BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (BoolMatrixExpr.Min(X.AsExpr, Y.AsExpr)) = BoolMatrix.Min(X, Y)

    [<Property>]
    let ``Min BoolMatrixExpr BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (BoolMatrixExpr.Min(X.AsExpr, Y)) = BoolMatrix.Min(X, Y)

    [<Property>]
    let ``Min BoolMatrix BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (BoolMatrixExpr.Min(X, Y.AsExpr)) = BoolMatrix.Min(X, Y)

    [<Property>]
    let ``Min BoolMatrixExpr scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (BoolMatrixExpr.Min(X.AsExpr, a)) = BoolMatrix.Min(X, a))

    [<Property>]
    let ``Min scalar BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (BoolMatrixExpr.Min(a, X.AsExpr)) = BoolMatrix.Min(a, X))

    [<Property>]
    let ``Max BoolMatrixExpr BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (BoolMatrixExpr.Max(X.AsExpr, Y.AsExpr)) = BoolMatrix.Max(X, Y)

    [<Property>]
    let ``Max BoolMatrixExpr BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (BoolMatrixExpr.Max(X.AsExpr, Y)) = BoolMatrix.Max(X, Y)

    [<Property>]
    let ``Max BoolMatrix BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (BoolMatrixExpr.Max(X, Y.AsExpr)) = BoolMatrix.Max(X, Y)

    [<Property>]
    let ``Max BoolMatrixExpr scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (BoolMatrixExpr.Max(X.AsExpr, a)) = BoolMatrix.Max(X, a))

    [<Property>]
    let ``Max scalar BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (BoolMatrixExpr.Max(a, X.AsExpr)) = BoolMatrix.Max(a, X))



    [<Property>]
    let ``BoolMatrixExpr .&& BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .&& Y.AsExpr) = (X .&& Y)

    [<Property>]
    let ``BoolMatrixExpr .&& BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .&& Y) = (X .&& Y)

    [<Property>]
    let ``BoolMatrix .&& BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .&& Y.AsExpr) = (X .&& Y)

    [<Property>]
    let ``BoolMatrixExpr .&& scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .&& a) = (X .&& a))

    [<Property>]
    let ``scalar .&& BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .&& X.AsExpr) = (a .&& X))


    [<Property>]
    let ``BoolMatrixExpr .|| BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .|| Y.AsExpr) = (X .|| Y)

    [<Property>]
    let ``BoolMatrixExpr .|| BoolMatrix`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X.AsExpr .|| Y) = (X .|| Y)

    [<Property>]
    let ``BoolMatrix .|| BoolMatrixExpr`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        eval (X .|| Y.AsExpr) = (X .|| Y)

    [<Property>]
    let ``BoolMatrixExpr .|| scalar`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .|| a) = (X .|| a))

    [<Property>]
    let ``scalar .|| BoolMatrixExpr`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        (x.Length > 0) ==> lazy(eval (a .|| X.AsExpr) = (a .|| X))

    [<Property>]
    let ``Not BoolMatrixExpr`` (x : bool[,]) =
        let X = new BoolMatrix(x)
        eval (BoolMatrixExpr.Not X.AsExpr) = (BoolMatrix.Not X)

    [<Property>]
    let ``EvalIn Not BoolMatrixExpr`` (x : bool[,]) =
        let X = new BoolMatrix(x)
        let res = BoolMatrix.Not X
        let evalRes = evalIn X (BoolMatrixExpr.Not X.AsExpr)
        X = res

    [<Fact>]
    let ``EvalIn(expr, None) throws arg exn if expr has no matching elementwise size``() =
        let x = new BoolMatrix(2,3,true)
        let y = new BoolMatrix(3,2,false)
        Assert.Throws<ArgumentException>(fun () -> BoolMatrixExpr.EvalIn(x.AsExpr .&& y, None) |> ignore)

    [<Fact>]
    let ``EvalIn(expr, Some v) throws arg exn if v has no matching elementwise size``() =
        let x = new BoolMatrix(2,3,true)
        let y = new BoolMatrix(2,3,false)
        let res = new BoolMatrix(3,2, false)
        Assert.Throws<ArgumentException>(fun () -> BoolMatrixExpr.EvalIn(x.AsExpr .&& y, Some res) |> ignore) 

