namespace FCore.MatlabTests

open FCore
open FCore.Math
open FCore.BasicStats
open FCore.Random
open FCore.LinearAlgebra
open FCore.ExplicitConversion
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module LinearAlgebra =

    let app = new MLAppClass()
    do app.Visible <- 0

    let rnd = new Random()

    let rng = new MT19937Rng()

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[,]) (y :float[,])  = epsEqualArray2D x y epsEqualFloat eps

    [<Property>]
    let ``Transpose in place``(v : float[,]) =
            setMatrix app "v" v
            app.Execute("res = v';") |> ignore
            let res = getMatrix app "res"
            let v = new Matrix(v)
            v.Transpose()
            epsEqual 0.0 (v.ToArray2D()) res    
            
    [<Property>]
    let ``Transpose``(v : float[,]) =
            setMatrix app "v" v
            app.Execute("res = v';") |> ignore
            let res = getMatrix app "res"
            let v = new Matrix(v)
            let y = transpose v
            epsEqual 0.0 (y.ToArray2D()) res     

    [<Fact>]
    let ``chol``() =
        let r = app.Execute("a = gallery('lehmer', 100);
                             b = chol(a);")
        let x = getMatrix app "a"
        let y = getMatrix app "b"
        let a = new Matrix(x)
        let b = new Matrix(y)
        let res = chol a
        epsEqual 1e-14 (res.ToArray2D()) y |> should be True

    [<Fact>]
    let ``cholinv``() =
        let r = app.Execute("a = gallery('lehmer', 100);
                             b = inv(a);")
        let x = getMatrix app "a"
        let y = getMatrix app "b"
        let a = new Matrix(x)
        let b = new Matrix(y)
        let res = cholInv a
        epsEqual 1e-10 (res.ToArray2D()) y |> should be True

    [<Fact>]
    let ``cholSolve``() =
        let r = app.Execute("a = gallery('lehmer', 100);
                             b = rand(100, 1);
                             c = a \ b;")
        let x = getMatrix app "a"
        let y = getMatrix app "b"
        let z = getMatrix app "c"
        let a = new Matrix(x)
        let b = new Matrix(y)
        let c = new Matrix(z)
        let res = cholSolve a  b
        epsEqual 1e-11 (res.ToArray2D()) z |> should be True

    [<Property>]
    let ``lu``(a : float[,]) =
        (a.Length > 0) ==> 
                          lazy(
                                let a = a |> Array2D.map (fun x -> if Double.IsNaN(x) || Double.IsInfinity(x) then 1.0 else x)
                                setMatrix app "a" a
                                let r = app.Execute("[l, u, p] = lu(a);")
                                let x = new Matrix(a)
                                let y = getMatrix app  "l"
                                let z = getMatrix app  "u"
                                let s = getMatrix app  "p"
                                let (l, u, p) = lu(x)
                                let rows = x.RowCount
                                let eye : Matrix = I rows rows
                                let v = eye.[p, {0..rows-1}]
                                (epsEqual 1e-14 (l.ToArray2D()) y) && (epsEqual 1e-13 (u.ToArray2D()) z) && (epsEqual 0.0 (v.ToArray2D()) s)                          
                              )


    [<Fact>]
    let ``luInv``() =
        let r = app.Execute("a = rand(200,200);
                             b = inv(a);")
        let x = getMatrix app  "a"
        let y = getMatrix app  "b"
        let a = new Matrix(x)
        let res = luInv a
        epsEqual 1e-11 (res.ToArray2D()) y |> should be True

    [<Fact>]
    let ``luSolve``() =
        let r = app.Execute("a = rand(100, 100);
                             b = rand(100, 1);
                             c = a \ b;")
        let x = getMatrix app  "a"
        let y = getMatrix app  "b"
        let z = getMatrix app  "c"
        let a = new Matrix(x)
        let b = new Matrix(y)
        let res = luSolve a b
        epsEqual 1e-11 (res.ToArray2D()) z |> should be True   
        
    [<Fact>]
    let ``QR more rows`` () =
        let r = app.Execute("a = rand(1000,100);
                             [q, r] = qr(a, 0);")
        let x = getMatrix app  "a"
        let y = getMatrix app  "q"
        let z = getMatrix app  "r"
        let (q, r) = qr (new Matrix(x))
        ((epsEqual 1e-10 (q.ToArray2D()) y) && (epsEqual 1e-10 (r.ToArray2D()) z)) |> should be True

    [<Fact>]
    let ``QR more cols`` () =
        let r = app.Execute("a = rand(100,1000);
                             [q, r] = qr(a, 0);")
        let x = getMatrix app  "a"
        let y = getMatrix app  "q"
        let z = getMatrix app  "r"
        let (q, r) = qr (new Matrix(x))
        ((epsEqual 1e-10 (q.ToArray2D()) y) && (epsEqual 1e-10 (r.ToArray2D()) z)) |> should be True

    [<Fact>]
    let ``QR solveFull`` () =
        let r = app.Execute("a = rand(500, 100);
                             b = rand(500, 1);
                             c = a \ b;")
        let x = getMatrix app  "a"
        let y = getMatrix app  "b"
        let z = getMatrix app  "c"
        let res = qrSolveFull (new Matrix(x)) (new Matrix(y))
        epsEqual 1e-13 (res.ToArray2D()) z |> should be True

    [<Fact>]
    let ``QR solve`` () =
        let r = app.Execute("a = reshape(2:13, 4, 3);
                             a(:, 1) = 0;
                             b = reshape(1:8, 4, 2);
                             c = a \ b;")
        let x = getMatrix app  "a"
        let y = getMatrix app  "b"
        let z = getMatrix app  "c"
        let (res, rank) = qrSolve (new Matrix(x)) (new Matrix(y)) 1e-15
        rank |> should equal 2
        res == new Matrix(z) |> should be True

    [<Fact>]
    let ``Svd solve`` () =
        let r = app.Execute("a = reshape(2:13, 4, 3);
                             a(:, 1) = 0;
                             b = reshape(1:8, 4, 2);
                             c = a \ b;")
        let x = getMatrix app  "a"
        let y = getMatrix app  "b"
        let z = getMatrix app  "c"
        let (res, rank) = svdSolve (new Matrix(x)) (new Matrix(y)) 1e-15
        rank |> should equal 2
        epsEqual 1e-13 (res.ToArray2D()) z |> should be True

    [<Fact>]
    let ``SVD more rows`` () =
        let r = app.Execute(" a=[9 4;6 8;2 7];
                             [u, s, v] = svd(a, 0);")
        let x = getMatrix app  "a"
        let U = getMatrix app  "u"
        let S = getMatrix app  "s"
        let V = getMatrix app  "v"
        let (u, s, vt) = svd (new Matrix(x))
        u == new Matrix(U) |> should be True
        diag s 0 == new Matrix(S) |> should be True
        transpose(vt) == new Matrix(V) |> should be True

    [<Fact>]
    let ``SVD more cols`` () =
        let r = app.Execute(" a=[9 2 8; 6 4 7];
                             [u, s, v] = svd(a, 0);")
        let x = getMatrix app  "a"
        let U = getMatrix app  "u"
        let S = getMatrix app  "s"
        let V = getMatrix app  "v"
        let (u, s, vt) = svd (new Matrix(x))
        epsEqual 1e-14 (u.ToArray2D()) U |> should be True
        diag s 0 == (new Matrix(S)).[0..,0..1] |> should be True
        transpose(vt) == (new Matrix(V)).[0..,0..1] |> should be True

    [<Fact>]
    let ``Singular values more rows`` () =
        let r = app.Execute("a = rand(1000,200);
                             s = svd(a, 0);")
        let x = getMatrix app  "a"
        let y = getVector app  "s"
        let svd = svdValues (new Matrix(x))
        epsEqualArray (svd.ToArray()) y epsEqualFloat 1e-13 |> should be True

    [<Fact>]
    let ``Singular values more cols`` () =
        let r = app.Execute("a = rand(200,1000);
                             s = svd(a, 0);")
        let x = getMatrix app  "a"
        let y = getVector app  "s"
        let svd = svdValues (new Matrix(x))
        epsEqualArray (svd.ToArray()) y epsEqualFloat 1e-13 |> should be True

    [<Fact>]
    let ``Eigen decompose`` () =
        let r = app.Execute("a = [1 3 5;3 2 4;5 4 1];
                             [b,c] = eig(a);")
        let x = getMatrix app  "a"
        let y = getMatrix app  "b"
        let z = getMatrix app  "c"
        let (vect, values) = eig (new Matrix(x))
        let values = diag values 0
        epsEqual 1e-15 (values.ToArray2D()) z |> should be True
        epsEqual 1e-15 (vect.ToArray2D()) y |> should be True

    [<Fact>]
    let ``Eigen values`` () =
        let r = app.Execute("a = [1 3 5;3 2 4;5 4 1];
                             b = eig(a);")
        let x = getMatrix app  "a"
        let y = getVector app  "b"
        let v = eigValues (new Matrix(x))
        epsEqualArray (v.ToArray()) y epsEqualFloat 1e-15 |> should be True




