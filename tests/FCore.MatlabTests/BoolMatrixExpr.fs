namespace FCore.MatlabTests

open FCore
open FCore.Math
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open FCore.ExplicitConversion

module BoolMatrixExpr = 

    let rnd = new Random()

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
    let ``Not Not BoolMatrixExpr`` (x : bool[,]) =
        let X = new BoolMatrix(x)
        eval (X.AsExpr |> BoolMatrixExpr.Not |> BoolMatrixExpr.Not) = X

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

    [<Property>]
    let ``Eval scalar Var`` (a : bool) =
        let x = new BoolMatrix(a)
        eval x.AsExpr = x

    [<Property>]
    let ``Eval scalar UnaryFunction`` (a : bool) =
        let x = new BoolMatrix(a)
        eval (x.AsExpr |> BoolMatrixExpr.Not |> BoolMatrixExpr.Not |> BoolMatrixExpr.Not) = new BoolMatrix(a |> not |> not |> not)

    [<Property>]
    let ``Eval scalar BinaryFunction`` (a1 : bool) (a2 : bool) (b1 : bool) (b2 : bool) =
        let x1 = new BoolMatrix(a1)
        let x2 = new BoolMatrix(a2)
        let y1 = new BoolMatrix(b1)
        let y2 = new BoolMatrix(b2)
        eval ((x1.AsExpr .&& x2) .|| (y1.AsExpr .&& y2)) = new BoolMatrix( (a1 && a2) || (b1 && b2))

    [<Property>]
    let ``Eval scalar BinaryVectorFunction`` (a : float) (b : float) =
        let x = new Matrix(a)
        let y = new Matrix(b)
        eval  (-(x.AsExpr) .< -(y.AsExpr)) = new BoolMatrix(-a < -b)

    [<Property>]
    let ``Eval scalar IfFunction`` (a : bool) (b : bool) (c : bool) =
        let x = new BoolMatrix(a)
        let y = new BoolMatrix(b)
        let z = new BoolMatrix(c)
        eval (iif (BoolMatrixExpr.Not(x.AsExpr)) (BoolMatrixExpr.Not(y.AsExpr)) (BoolMatrixExpr.Not(z.AsExpr))) = new BoolMatrix(if not a then not b else not c)

    [<Property>]
    let ``eval (scalar && BoolMatrix) || BoolMatrix`` (v : (bool*bool)[,]) (a : bool) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (v.Length > 0) ==> lazy(eval ((a .&& X.AsExpr) .|| Y) = ((a .&& X) .|| Y))

    [<Property>]
    let ``eval (BoolMatrix && (scalar || BoolMatrix)`` (v : (bool*bool)[,]) (a : bool) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (v.Length > 0) ==> lazy(eval (X .&& (a .|| Y.AsExpr)) = (X .&& (a .|| Y)))

    [<Property>]
    let ``eval iif (a && X) Y b`` (v : (bool*bool)[,]) (a : bool) (b : bool) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let aX = a .&& X
        (v.Length > 0) ==> lazy(let res = eval (iif (a .&& X.AsExpr) Y !!b) in res.ToArray2D() |> Array2D.mapi (fun i j x -> res.[i,j] = if aX.[i,j] then Y.[i,j] else b) = Array2D.create res.RowCount res.ColCount true)

    [<Property>]
    let ``eval iif X a (b && Y)`` (v : (bool*bool)[,]) (a : bool) (b : bool) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let bY = b .&& Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr !!a (b .&& Y)) in res.ToArray2D() |> Array2D.mapi (fun i j x -> res.[i,j] = if X.[i,j] then a else bY.[i,j]) = Array2D.create res.RowCount res.ColCount true)


    [<Property>]
    let ``eval iif X (a && Y) b`` (v : (bool*bool)[,]) (a : bool) (b : bool) =
        let x = v |> Array2D.map fst
        let y = v |> Array2D.map snd
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let aY = a .&& Y
        (v.Length > 0) ==> lazy(let res = eval (iif X.AsExpr (a .&& Y) !!b) in res.ToArray2D() |> Array2D.mapi (fun i j x -> res.[i,j] = if X.[i,j] then aY.[i,j] else b) = Array2D.create res.RowCount res.ColCount true)


    [<Fact>]
    let ``eval large vector`` () =
        use x = new BoolMatrix(4121232, 5, fun i j -> rnd.NextDouble() < 0.5)
        use y = new BoolMatrix(4121232, 5, fun i j -> rnd.NextDouble() < 0.5)
        eval (BoolMatrixExpr.Min(x.AsExpr .&& y.AsExpr, x.AsExpr .|| y.AsExpr) |> BoolMatrixExpr.Not) = (BoolMatrix.Min(x .&& y, x .|| y) |> BoolMatrix.Not) 


