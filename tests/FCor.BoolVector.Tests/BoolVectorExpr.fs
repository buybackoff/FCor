namespace FCor.BoolVector.Tests

open FCor
open FCor.Math
open FCor.ExplicitConversion
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolVectorExpr = 

    let rnd = new Random()

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

    [<Property>]
    let ``Not Not BoolVectorExpr`` (x : bool[]) =
        let X = new BoolVector(x)
        eval (X.AsExpr |> BoolVectorExpr.Not |> BoolVectorExpr.Not) = X

    [<Property>]
    let ``EvalIn Not BoolVectorExpr`` (x : bool[]) =
        let X = new BoolVector(x)
        let res = BoolVector.Not X
        evalIn (BoolVectorExpr.Not X.AsExpr) X
        X = res

    [<Fact>]
    let ``EvalIn(expr, None) throws arg exn if expr has no matching elementwise length``() =
        let x = new BoolVector(6,true)
        let y = new BoolVector(5,false)
        Assert.Throws<ArgumentException>(fun () -> BoolVectorExpr.EvalIn(x.AsExpr .&& y, None) |> ignore)

    [<Fact>]
    let ``EvalIn(expr, Some v) throws arg exn if v has no matching elementwise length``() =
        let x = new BoolVector(6,true)
        let y = new BoolVector(6,false)
        let res = new BoolVector(5, false)
        Assert.Throws<ArgumentException>(fun () -> BoolVectorExpr.EvalIn(x.AsExpr .&& y, Some res) |> ignore) 

    [<Property>]
    let ``Eval scalar Var`` (a : bool) =
        let x = new BoolVector(a)
        eval x.AsExpr = x

    [<Property>]
    let ``Eval scalar UnaryFunction`` (a : bool) =
        let x = new BoolVector(a)
        eval (x.AsExpr |> BoolVectorExpr.Not |> BoolVectorExpr.Not |> BoolVectorExpr.Not) = new BoolVector(a |> not |> not |> not)

    [<Property>]
    let ``Eval scalar BinaryFunction`` (a1 : bool) (a2 : bool) (b1 : bool) (b2 : bool) =
        let x1 = new BoolVector(a1)
        let x2 = new BoolVector(a2)
        let y1 = new BoolVector(b1)
        let y2 = new BoolVector(b2)
        eval ((x1.AsExpr .&& x2) .|| (y1.AsExpr .&& y2)) = new BoolVector( (a1 && a2) || (b1 && b2))

    [<Property>]
    let ``Eval scalar BinaryVectorFunction`` (a : float) (b : float) =
        let x = new Vector(a)
        let y = new Vector(b)
        eval  (-(x.AsExpr) .< -(y.AsExpr)) = new BoolVector(-a < -b)

    [<Property>]
    let ``Eval scalar IfFunction`` (a : bool) (b : bool) (c : bool) =
        let x = new BoolVector(a)
        let y = new BoolVector(b)
        let z = new BoolVector(c)
        eval (iif (BoolVectorExpr.Not(x.AsExpr)) (BoolVectorExpr.Not(y.AsExpr)) (BoolVectorExpr.Not(z.AsExpr))) = new BoolVector(if not a then not b else not c)

    [<Property>]
    let ``eval (scalar && BoolVector) || BoolVector`` (v : (bool*bool)[]) (a : bool) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (v.Length > 0) ==> lazy(eval ((a .&& X.AsExpr) .|| Y) = ((a .&& X) .|| Y))

    [<Property>]
    let ``eval (BoolVector && (scalar || BoolVector)`` (v : (bool*bool)[]) (a : bool) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (v.Length > 0) ==> lazy(eval (X .&& (a .|| Y.AsExpr)) = (X .&& (a .|| Y)))

    [<Property>]
    let ``eval iif (a && X) Y b`` (v : (bool*bool)[]) (a : bool) (b : bool) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let aX = a .&& X
        (v.Length > 0) ==> lazy(let res = eval (iif (a .&& X.AsExpr) Y !!b) in res.ToArray() |> Array.mapi (fun i x -> res.[i] = if aX.[i] then Y.[i] else b) |> Array.fold (&&) true)

    [<Property>]
    let ``eval iif X a (b && Y)`` (v : (bool*bool)[]) (a : bool) (b : bool) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let bY = b .&& Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr !!a (b .&& Y)) in res.ToArray() |> Array.mapi (fun i x -> res.[i] = if X.[i] then a else bY.[i]) |> Array.fold (&&) true)


    [<Property>]
    let ``eval iif X (a && Y) b`` (v : (bool*bool)[]) (a : bool) (b : bool) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let aY = a .&& Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr (a .&& Y) !!b) in res.ToArray() |> Array.mapi (fun i x -> res.[i] = if X.[i] then aY.[i] else b) |> Array.fold (&&) true)

    [<Property>]
    let ``eval iif X a b`` (v : bool[]) (a : bool) (b : bool) =
        let X = new BoolVector(v)
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr a b) in res.ToArray() |> Array.mapi (fun i x -> res.[i] = if X.[i] then a else b) |> Array.fold (&&) true)


    [<Fact>]
    let ``eval large vector`` () =
        use x = new BoolVector(21234567, fun i -> rnd.NextDouble() < 0.5)
        use y = new BoolVector(21234567, fun i -> rnd.NextDouble() < 0.5)
        eval (BoolVectorExpr.Min(x.AsExpr .&& y.AsExpr, x.AsExpr .|| y.AsExpr) |> BoolVectorExpr.Not) = (BoolVector.Min(x .&& y, x .|| y) |> BoolVector.Not) 
