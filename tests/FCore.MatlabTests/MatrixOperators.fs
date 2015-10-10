namespace FCore.MatlabTests

open FCore
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module MatrixOperators = 
    let app = new MLAppClass()
    do app.Visible <- 0

    let rnd = new Random()

    let inline (<=>) (x : float[,]) (y :float[,]) = epsEqualArray2D x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[,]) (y :float[,])  = epsEqualArray2D x y epsEqualFloat eps

    [<Property>]
    let ``X == X`` (x : float[,]) =
        let X = new Matrix(x)
        let hasNaN = x |> array2DExists Double.IsNaN
        ((X == X) = not hasNaN) .&. ((X == Matrix.Copy(X)) = not hasNaN)

    [<Property>]
    let ``X = X`` (x : float[,]) =
        let X = new Matrix(x)
        let hasNaN = x |> array2DExists Double.IsNaN
        ((X = X) = not hasNaN) .&. ((X = Matrix.Copy(X)) = not hasNaN)

    [<Property(MaxTest=1000)>]
    let ``X != Y if not same shape and data`` (x : float[,]) (y :  float[,]) =
        let x = x |> fixEmpty
        let y = y |> fixEmpty
        let X = new Matrix(x) 
        let Y = new Matrix(y) 
        x <> y ==> (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X <> Y if not same shape and data`` (x : float[,]) (y :  float[,]) =
        let x = x |> fixEmpty
        let y = y |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        x <> y ==> (X <> Y)

    [<Property(MaxTest=1000)>]
    let ``X == Y = not X != Y`` (x : float[,]) (y :  float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X == Y) = not (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X = Y = not X <> Y`` (x : float[,]) (y :  float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X = Y) = not (X <> Y)

    [<Property>]
    let ``Matrix(a) == a`` (a : float) =
        let A = new Matrix(a)
        (not <| Double.IsNaN(a)) ==> ((A == a) .&. (a == A))

    [<Property(MaxTest=1000)>]
    let ``X == a = not X != a`` (x : float[,]) (a :  float) =
        let X = new Matrix(x)
        (X == a) = not (X != a)

    [<Property(MaxTest=1000)>]
    let ``a == X = not a != X`` (x : float[,]) (a :  float) =
        let X = new Matrix(x)
        (a == X) = not (a != X)


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .< Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .< Y))

    [<Property>]
    let ``X .< Y`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X .< Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 < z2)) 

    [<Property>]
    let ``X .< a`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X .< a) == (X .< A)) .&. ((X .< a).ToArray2D() = (x |> Array2D.map (fun z -> z < a)))

    [<Property>]
    let ``a .< X`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a .< X) == (A .< X)) .&. ((a .< X).ToArray2D() = (x |> Array2D.map (fun z -> a < z)))



    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .<= Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .<= Y))

    [<Property>]
    let ``X .<= Y`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X .<= Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 <= z2)) 

    [<Property>]
    let ``X .<= a`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X .<= a) == (X .<= A)) .&. ((X .<= a).ToArray2D() = (x |> Array2D.map (fun z -> z <= a)))

    [<Property>]
    let ``a .<= X`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a .<= X) == (A .<= X)) .&. ((a .<= X).ToArray2D() = (x |> Array2D.map (fun z -> a <= z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .> Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .> Y))

    [<Property>]
    let ``X .> Y`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X .> Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 > z2)) 

    [<Property>]
    let ``X .> a`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X .> a) == (X .> A)) .&. ((X .> a).ToArray2D() = (x |> Array2D.map (fun z -> z > a)))

    [<Property>]
    let ``a .> X`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a .> X) == (A .> X)) .&. ((a .> X).ToArray2D() = (x |> Array2D.map (fun z -> a > z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .>= Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .>= Y))

    [<Property>]
    let ``X .>= Y`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X .>= Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 >= z2)) 

    [<Property>]
    let ``X .>= a`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X .>= a) == (X .>= A)) .&. ((X .>= a).ToArray2D() = (x |> Array2D.map (fun z -> z >= a)))

    [<Property>]
    let ``a .>= X`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a .>= X) == (A .>= X)) .&. ((a .>= X).ToArray2D() = (x |> Array2D.map (fun z -> a >= z)))
        
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .= Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .= Y))

    [<Property>]
    let ``X .= Y`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X .= Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 = z2)) 

    [<Property>]
    let ``X .= a`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X .= a) == (X .= A)) .&. ((X .= a).ToArray2D() = (x |> Array2D.map (fun z -> z = a)))

    [<Property>]
    let ``a .= X`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a .= X) == (A .= X)) .&. ((a .= X).ToArray2D() = (x |> Array2D.map (fun z -> a = z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .<> Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .<> Y))

    [<Property>]
    let ``X .<> Y`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (X .<> Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 <> z2)) 

    [<Property>]
    let ``X .<> a`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X .<> a) == (X .<> A)) .&. ((X .<> a).ToArray2D() = (x |> Array2D.map (fun z -> z <> a)))

    [<Property>]
    let ``a .<> X`` (x : float[,]) (a : float) =
        let x = x |> fixEmpty
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a .<> X) == (A .<> X)) .&. ((a .<> X).ToArray2D() = (x |> Array2D.map (fun z -> a <> z)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if size mismatch in X * Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        ((cols1 <> rows2) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X * Y))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if size mismatch in X ^* Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        ((rows1 <> rows2) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X ^* Y))

    [<Property>]
    let ``Operator *``(x : float[,]) =
        let x = x |> fixEmpty
        setMatrix app "a" x
        let b = Array2D.init (x.GetLength(1)) (rnd.Next(100)) (fun i j -> rnd.NextDouble())
        setMatrix app "b" b
        let a = new Matrix(x)
        let b = new Matrix(b)
        let r = app.Execute("c = a * b;")
        let z = getMatrix app "c"
        epsEqual 1e-13 ((a * b).ToArray2D()) z

    [<Property>]
    let ``Operator * vector``(x : float[,]) =
        let x = x |> fixEmpty
        setMatrix app "a" x
        let b = Array.init (x.GetLength(1)) (fun i -> rnd.NextDouble())
        setVector app "b" b
        let a = new Matrix(x)
        let b = new Vector(b)
        let r = app.Execute("c = a * b;")
        let z = getMatrix app "c"
        epsEqual 1e-13 ((a * b).ToArray2D()) z

    [<Property>]
    let ``Operator ^*``(x : float[,]) =
        let x = x |> fixEmpty
        setMatrix app "a" x
        let b = Array2D.init (x.GetLength(0)) (rnd.Next(100)) (fun i j -> rnd.NextDouble())
        setMatrix app "b" b
        let a = new Matrix(x)
        let b = new Matrix(b)
        let r = app.Execute("c = a' * b;")
        let z = getMatrix app "c"
        epsEqual 1e-11 ((a ^* b).ToArray2D()) z


                    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X + Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X + Y))

    [<Property>]
    let ``Operator +``(v : (float*float)[,]) =
        let v1 = v |> Array2D.map fst |> fixEmpty
        let v2 = v |> Array2D.map snd |> fixEmpty 
        setMatrix app "v1" v1
        setMatrix app "v2" v2
        app.Execute("res = v1 + v2;") |> ignore
        let res = getMatrix app "res"
        let v1 = new Matrix(v1)
        let v2 = new Matrix(v2)
        (v1 + v2).ToArray2D() <=> res


    [<Property>]
    let ``X + a`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X + a).ToArray2D() <=> (X + A).ToArray2D()) .&. ((X + a).ToArray2D() <=> (x |> Array2D.map (fun z -> z + a)))

    [<Property>]
    let ``a + X`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a + X).ToArray2D() <=> (A + X).ToArray2D()) .&. ((a + X).ToArray2D() <=> (x |> Array2D.map (fun z -> a + z)))



    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X - Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X - Y))

    [<Property>]
    let ``Operator -``(v : (float*float)[,]) =
        let v1 = v |> Array2D.map fst |> fixEmpty
        let v2 = v |> Array2D.map snd |> fixEmpty 
        setMatrix app "v1" v1
        setMatrix app "v2" v2
        app.Execute("res = v1 - v2;") |> ignore
        let res = getMatrix app "res"
        let v1 = new Matrix(v1)
        let v2 = new Matrix(v2)
        (v1 - v2).ToArray2D() <=> res


    [<Property>]
    let ``X - a`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X - a).ToArray2D() <=> (X - A).ToArray2D()) .&. ((X - a).ToArray2D() <=> (x |> Array2D.map (fun z -> z - a)))

    [<Property>]
    let ``a - X`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a - X).ToArray2D() <=> (A - X).ToArray2D()) .&. ((a - X).ToArray2D() <=> (x |> Array2D.map (fun z -> a - z)))
    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .* Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .* Y))

    [<Property>]
    let ``Operator .*``(v : (float*float)[,]) =
        let v1 = v |> Array2D.map fst |> fixEmpty
        let v2 = v |> Array2D.map snd |> fixEmpty
        setMatrix app "v1" v1
        setMatrix app "v2" v2
        app.Execute("res = v1 .* v2;") |> ignore
        let res = getMatrix app "res"
        let v1 = new Matrix(v1)
        let v2 = new Matrix(v2)
        (v1 .* v2).ToArray2D() <=> res


    [<Property>]
    let ``X .* a`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((X .* a).ToArray2D() <=> (X .* A).ToArray2D()) .&. ((X .* a).ToArray2D() <=> (x |> Array2D.map (fun z -> z * a)))

    [<Property>]
    let ``a .* X`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((a .* X).ToArray2D() <=> (A .* X).ToArray2D()) .&. ((a .* X).ToArray2D() <=> (x |> Array2D.map (fun z -> a * z)))
    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X ./ Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X ./ Y))

    [<Property>]
    let ``Operator ./``(v : (float*float)[,]) =
        let v1 = v |> Array2D.map fst |> fixEmpty
        let v2 = v |> Array2D.map snd |> fixEmpty 
        setMatrix app "v1" v1
        setMatrix app "v2" v2
        app.Execute("res = v1 ./ v2;") |> ignore
        let res = getMatrix app "res"
        let v1 = new Matrix(v1)
        let v2 = new Matrix(v2)
        epsEqual 1e-14 ((v1 ./ v2).ToArray2D()) res


    [<Property>]
    let ``X ./ a`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        (epsEqual 1e-14 ((X ./ a).ToArray2D()) ((X ./ A).ToArray2D())) .&. (epsEqual 1e-14 ((X ./ a).ToArray2D()) (x |> Array2D.map (fun z -> z / a)))

    [<Property>]
    let ``a ./ X`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        (epsEqual 1e-14 ((a ./ X).ToArray2D()) ((A ./ X).ToArray2D())) .&. (epsEqual 1e-14 ((a ./ X).ToArray2D()) (x |> Array2D.map (fun z -> a / z)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .^ Y`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .^ Y))

    [<Property>]
    let ``Operator .^``(v : (float*float)[,]) =
        let v1 = v |> Array2D.map (fst>>abs) |> Array2D.map (fun x -> if Double.IsNaN(x) || Double.IsInfinity(x) then 0.0 else x) |> fixEmpty
        let v2 = v |> Array2D.map snd |> Array2D.map (fun x -> if Double.IsNaN(x) || Double.IsInfinity(x) then 0.0 else x) |> fixEmpty
        setMatrix app "v1" v1
        setMatrix app "v2" v2
        app.Execute("res = v1 .^ v2;") |> ignore
        let res = new Matrix(getMatrix app "res")
        let v1 = new Matrix(v1)
        let v2 = new Matrix(v2)
        let v3 = v1 .^ v2
        epsEqual 1e-14 (v3.ToArray2D()) (res.ToArray2D())

    [<Property>]
    let ``X .^ a`` (x : float[,]) (a : float) =
        let x = x |> Array2D.map (fun x -> if Double.IsNaN(x) then 1.0 else abs(x))
        let a = if Double.IsNaN(a) then 1.0 else a
        let X = new Matrix(x)
        let A = new Matrix(a)
        (epsEqual 1e-14 ((X .^ a).ToArray2D()) ((X .^ A).ToArray2D())) .&. (epsEqual 1e-15 ((X .^ a).ToArray2D()) (x |> Array2D.map (fun z -> if a = 0.0 then 1.0 elif z = 1.0 then 1.0 else z ** a)))

    [<Property>]
    let ``X .^ n`` (x : float[,]) (n : int) =
        let a = float n
        let x = x |> Array2D.map (fun x -> if Double.IsNaN(x) then 1.0 else abs(x))
        let a = if Double.IsNaN(a) then 1.0 else a
        let X = new Matrix(x)
        let A = new Matrix(a)
        (epsEqual 1e-15 ((X .^ n).ToArray2D()) (x |> Array2D.map (fun z -> if a = 0.0 then 1.0 elif z = 1.0 then 1.0 else z ** a)))


    [<Property>]
    let ``a .^ X`` (x : float[,]) (a : float) =
        let a = if Double.IsNaN(a) then 1.0 else abs a
        let x = x |> Array2D.map (fun x -> if Double.IsNaN(x) then 1.0 else x)
        let X = new Matrix(x)
        let A = new Matrix(a)
        (epsEqual 1e-14 ((a .^ X).ToArray2D()) ((A .^ X).ToArray2D())) .&. (epsEqual 1e-15 ((a .^ X).ToArray2D()) (x |> Array2D.map (fun z -> if z = 0.0 then 1.0 elif a = 1.0 then 1.0 else a ** z)))

    [<Property>]
    let ``Operator unary -``(v : float[,]) = 
        setMatrix app "v" v
        app.Execute("res = -v;") |> ignore
        let res = getMatrix app "res"
        let V = new Matrix(v)
        (-V).ToArray2D() <=> res

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in Min(X, Y)`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(Matrix.Min(X, Y)))

    [<Property>]
    let ``Min(X, Y)`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (Matrix.Min(X, Y)).ToArray2D() <=> (y |> array2DZip x |> Array2D.map Math.Min)

    [<Property>]
    let ``Min(X, a)`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((Matrix.Min(X, a).ToArray2D()) <=> (Matrix.Min(X, A).ToArray2D())) .&. ((Matrix.Min(X, a)).ToArray2D() <=> (x |> Array2D.map (fun z -> min z a)))

    [<Property>]
    let ``Min(a, X)`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((Matrix.Min(a, X).ToArray2D()) <=> (Matrix.Min(A, X).ToArray2D())) .&. ((Matrix.Min(a, X)).ToArray2D() <=> (x |> Array2D.map (fun z -> min a z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in Max(X, Y)`` (x : float[,]) (y : float[,]) =
        let X = new Matrix(x)
        let Y = new Matrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(Matrix.Max(X, Y)))

    [<Property>]
    let ``Max(X, Y)`` (v : (float*float)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new Matrix(x)
        let Y = new Matrix(y)
        (Matrix.Max(X, Y)).ToArray2D() <=> (y |> array2DZip x |> Array2D.map Math.Max) 

    [<Property>]
    let ``Max(X, a)`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((Matrix.Max(X, a).ToArray2D()) <=> (Matrix.Max(X, A).ToArray2D())) .&. ((Matrix.Max(X, a)).ToArray2D() <=> (x |> Array2D.map (fun z -> max z a)))

    [<Property>]
    let ``Max(a, X)`` (x : float[,]) (a : float) =
        let X = new Matrix(x)
        let A = new Matrix(a)
        ((Matrix.Max(a, X).ToArray2D()) <=> (Matrix.Max(A, X).ToArray2D())) .&. ((Matrix.Max(a, X)).ToArray2D() <=> (x |> Array2D.map (fun z -> max a z)))

