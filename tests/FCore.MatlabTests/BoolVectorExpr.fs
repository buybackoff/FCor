namespace FCore.MatlabTests

open FCore
open FCore.Math
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolVectorExpr = 

    [<Property>]
    let ``BoolVectorExpr .< BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``BoolVectorExpr .< BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .< Y) = (X .< Y)

    [<Property>]
    let ``BoolVector .< BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .< Y.AsExpr) = (X .< Y)

    [<Property>]
    let ``BoolVectorExpr .< scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .< a) = (X .< a))

    [<Property>]
    let ``scalar .< BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .< X.AsExpr) = (a .< X))


    [<Property>]
    let ``BoolVectorExpr .<= BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``BoolVectorExpr .<= BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .<= Y) = (X .<= Y)

    [<Property>]
    let ``BoolVector .<= BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .<= Y.AsExpr) = (X .<= Y)

    [<Property>]
    let ``BoolVectorExpr .<= scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<= a) = (X .<= a))

    [<Property>]
    let ``scalar .<= BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .<= X.AsExpr) = (a .<= X))


    [<Property>]
    let ``BoolVectorExpr .> BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``BoolVectorExpr .> BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .> Y) = (X .> Y)

    [<Property>]
    let ``BoolVector .> BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .> Y.AsExpr) = (X .> Y)

    [<Property>]
    let ``BoolVectorExpr .> scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .> a) = (X .> a))

    [<Property>]
    let ``scalar .> BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .> X.AsExpr) = (a .> X))


    [<Property>]
    let ``BoolVectorExpr .>= BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``BoolVectorExpr .>= BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .>= Y) = (X .>= Y)

    [<Property>]
    let ``BoolVector .>= BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .>= Y.AsExpr) = (X .>= Y)

    [<Property>]
    let ``BoolVectorExpr .>= scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .>= a) = (X .>= a))

    [<Property>]
    let ``scalar .>= BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .>= X.AsExpr) = (a .>= X))


    [<Property>]
    let ``BoolVectorExpr .= BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``BoolVectorExpr .= BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .= Y) = (X .= Y)

    [<Property>]
    let ``BoolVector .= BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .= Y.AsExpr) = (X .= Y)

    [<Property>]
    let ``BoolVectorExpr .= scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .= a) = (X .= a))

    [<Property>]
    let ``scalar .= BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .= X.AsExpr) = (a .= X))


    [<Property>]
    let ``BoolVectorExpr .<> BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``BoolVectorExpr .<> BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .<> Y) = (X .<> Y)

    [<Property>]
    let ``BoolVector .<> BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .<> Y.AsExpr) = (X .<> Y)

    [<Property>]
    let ``BoolVectorExpr .<> scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .<> a) = (X .<> a))

    [<Property>]
    let ``scalar .<> BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .<> X.AsExpr) = (a .<> X))

    [<Property>]
    let ``Min BoolVectorExpr BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (BoolVectorExpr.Min(X.AsExpr, Y.AsExpr)) = BoolVector.Min(X, Y)

    [<Property>]
    let ``Min BoolVectorExpr BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (BoolVectorExpr.Min(X.AsExpr, Y)) = BoolVector.Min(X, Y)

    [<Property>]
    let ``Min BoolVector BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (BoolVectorExpr.Min(X, Y.AsExpr)) = BoolVector.Min(X, Y)

    [<Property>]
    let ``Min BoolVectorExpr scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (BoolVectorExpr.Min(X.AsExpr, a)) = BoolVector.Min(X, a))

    [<Property>]
    let ``Min scalar BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (BoolVectorExpr.Min(a, X.AsExpr)) = BoolVector.Min(a, X))

    [<Property>]
    let ``Max BoolVectorExpr BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (BoolVectorExpr.Max(X.AsExpr, Y.AsExpr)) = BoolVector.Max(X, Y)

    [<Property>]
    let ``Max BoolVectorExpr BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (BoolVectorExpr.Max(X.AsExpr, Y)) = BoolVector.Max(X, Y)

    [<Property>]
    let ``Max BoolVector BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (BoolVectorExpr.Max(X, Y.AsExpr)) = BoolVector.Max(X, Y)

    [<Property>]
    let ``Max BoolVectorExpr scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (BoolVectorExpr.Max(X.AsExpr, a)) = BoolVector.Max(X, a))

    [<Property>]
    let ``Max scalar BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (BoolVectorExpr.Max(a, X.AsExpr)) = BoolVector.Max(a, X))



    [<Property>]
    let ``BoolVectorExpr .&& BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .&& Y.AsExpr) = (X .&& Y)

    [<Property>]
    let ``BoolVectorExpr .&& BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .&& Y) = (X .&& Y)

    [<Property>]
    let ``BoolVector .&& BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .&& Y.AsExpr) = (X .&& Y)

    [<Property>]
    let ``BoolVectorExpr .&& scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .&& a) = (X .&& a))

    [<Property>]
    let ``scalar .&& BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .&& X.AsExpr) = (a .&& X))


    [<Property>]
    let ``BoolVectorExpr .|| BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .|| Y.AsExpr) = (X .|| Y)

    [<Property>]
    let ``BoolVectorExpr .|| BoolVector`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X.AsExpr .|| Y) = (X .|| Y)

    [<Property>]
    let ``BoolVector .|| BoolVectorExpr`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        eval (X .|| Y.AsExpr) = (X .|| Y)

    [<Property>]
    let ``BoolVectorExpr .|| scalar`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (X.AsExpr .|| a) = (X .|| a))

    [<Property>]
    let ``scalar .|| BoolVectorExpr`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        (x.Length > 0) ==> lazy(eval (a .|| X.AsExpr) = (a .|| X))

    [<Property>]
    let ``Not BoolVectorExpr`` (x : bool[]) =
        let X = new BoolVector(x)
        eval (BoolVectorExpr.Not X.AsExpr) = (BoolVector.Not X)

