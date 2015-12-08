namespace FCor.Vector.Tests

open FCor
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util
open System.Runtime.InteropServices

module VectorOperators = 
    let rnd = new Random()

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[]) (y :float[])  = epsEqualArray x y epsEqualFloat eps

    let regDouble x =
        if Double.IsNaN(x) || Double.IsNegativeInfinity(x) || Double.IsPositiveInfinity(x) || x = Double.MaxValue ||x = Double.MinValue || x = Double.Epsilon || x = -Double.Epsilon then rnd.NextDouble() else x

    [<Property>]
    let ``X == X`` (x : float[]) =
        let X = new Vector(x)
        let hasNaN = x |> Array.exists Double.IsNaN
        ((X == X) = not hasNaN) .&. ((X == Vector.Copy(X)) = not hasNaN)

    [<Property>]
    let ``X = X`` (x : float[]) =
        let X = new Vector(x)
        let hasNaN = x |> Array.exists Double.IsNaN
        ((X = X) = not hasNaN) .&. ((X = Vector.Copy(X)) = not hasNaN)

    [<Property(MaxTest=1000)>]
    let ``X != Y if not same length and data`` (x : float[]) (y :  float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        x <> y ==> (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X <> Y if not same length and data`` (x : float[]) (y :  float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        x <> y ==> (X <> Y)

    [<Property(MaxTest=1000)>]
    let ``X == Y = not X != Y`` (x : float[]) (y :  float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        (X == Y) = not (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X = Y = not X <> Y`` (x : float[]) (y :  float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        (X = Y) = not (X <> Y)

    [<Property>]
    let ``Vector(a) == a`` (a : float) =
        let A = new Vector(a)
        (not <| Double.IsNaN(a)) ==> ((A == a) .&. (a == A))

    [<Property(MaxTest=1000)>]
    let ``X == a = not X != a`` (x : float[]) (a :  float) =
        let X = new Vector(x)
        (X == a) = not (X != a)

    [<Property(MaxTest=1000)>]
    let ``a == X = not a != X`` (x : float[]) (a :  float) =
        let X = new Vector(x)
        (a == X) = not (a != X)


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .< Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .< Y))

    [<Property>]
    let ``X .< Y`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (X .< Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 < z2)) 

    [<Property>]
    let ``X .< a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X .< a) == (X .< A)) .&. ((X .< a).ToArray() = (x |> Array.map (fun z -> z < a)))

    [<Property>]
    let ``a .< X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a .< X) == (A .< X)) .&. ((a .< X).ToArray() = (x |> Array.map (fun z -> a < z)))



    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .<= Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .<= Y))

    [<Property>]
    let ``X .<= Y`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (X .<= Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 <= z2)) 

    [<Property>]
    let ``X .<= a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X .<= a) == (X .<= A)) .&. ((X .<= a).ToArray() = (x |> Array.map (fun z -> z <= a)))

    [<Property>]
    let ``a .<= X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a .<= X) == (A .<= X)) .&. ((a .<= X).ToArray() = (x |> Array.map (fun z -> a <= z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .> Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .> Y))

    [<Property>]
    let ``X .> Y`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (X .> Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 > z2)) 

    [<Property>]
    let ``X .> a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X .> a) == (X .> A)) .&. ((X .> a).ToArray() = (x |> Array.map (fun z -> z > a)))

    [<Property>]
    let ``a .> X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a .> X) == (A .> X)) .&. ((a .> X).ToArray() = (x |> Array.map (fun z -> a > z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .>= Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .>= Y))

    [<Property>]
    let ``X .>= Y`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (X .>= Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 >= z2)) 

    [<Property>]
    let ``X .>= a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X .>= a) == (X .>= A)) .&. ((X .>= a).ToArray() = (x |> Array.map (fun z -> z >= a)))

    [<Property>]
    let ``a .>= X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a .>= X) == (A .>= X)) .&. ((a .>= X).ToArray() = (x |> Array.map (fun z -> a >= z)))
        
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .= Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .= Y))

    [<Property>]
    let ``X .= Y`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (X .= Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 = z2)) 

    [<Property>]
    let ``X .= a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X .= a) == (X .= A)) .&. ((X .= a).ToArray() = (x |> Array.map (fun z -> z = a)))

    [<Property>]
    let ``a .= X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a .= X) == (A .= X)) .&. ((a .= X).ToArray() = (x |> Array.map (fun z -> a = z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .<> Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .<> Y))

    [<Property>]
    let ``X .<> Y`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (X .<> Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 <> z2)) 

    [<Property>]
    let ``X .<> a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X .<> a) == (X .<> A)) .&. ((X .<> a).ToArray() = (x |> Array.map (fun z -> z <> a)))

    [<Property>]
    let ``a .<> X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a .<> X) == (A .<> X)) .&. ((a .<> X).ToArray() = (x |> Array.map (fun z -> a <> z)))
        
        
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X * Y or empty`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) || (len1 = 0L) || (len2 = 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X * Y))

    [<Property>]
    let ``X * Y`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (v.Length > 0) ==> lazy(epsEqual 1e-14 [|X * Y|] [|(y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 * z2) |> Array.reduce (+))|])

                    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X + Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X + Y))

    [<Property>]
    let ``Operator +``(v : (float*float)[]) =
        let v1 = v |> Array.map fst
        let v2 = v |> Array.map snd 
        let app = getApp()
        setVector app "v1" v1
        setVector app "v2" v2
        app.Execute("res = v1 + v2;") |> ignore
        let res = getVector app "res"
        let v1 = new Vector(v1)
        let v2 = new Vector(v2)
        (v1 + v2).ToArray() <=> res


    [<Property>]
    let ``X + a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X + a).ToArray() <=> (X + A).ToArray()) .&. ((X + a).ToArray() <=> (x |> Array.map (fun z -> z + a)))

    [<Property>]
    let ``a + X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a + X).ToArray() <=> (A + X).ToArray()) .&. ((a + X).ToArray() <=> (x |> Array.map (fun z -> a + z)))



    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X - Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X - Y))

    [<Property>]
    let ``Operator -``(v : (float*float)[]) =
        let v1 = v |> Array.map fst
        let v2 = v |> Array.map snd 
        let app = getApp()
        setVector app "v1" v1
        setVector app "v2" v2
        app.Execute("res = v1 - v2;") |> ignore
        let res = getVector app "res"
        let v1 = new Vector(v1)
        let v2 = new Vector(v2)
        (v1 - v2).ToArray() <=> res


    [<Property>]
    let ``X - a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X - a).ToArray() <=> (X - A).ToArray()) .&. ((X - a).ToArray() <=> (x |> Array.map (fun z -> z - a)))

    [<Property>]
    let ``a - X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a - X).ToArray() <=> (A - X).ToArray()) .&. ((a - X).ToArray() <=> (x |> Array.map (fun z -> a - z)))
    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .* Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .* Y))

    [<Property>]
    let ``Operator .*``(v : (float*float)[]) =
        let v1 = v |> Array.map fst
        let v2 = v |> Array.map snd 
        let app = getApp()
        setVector app "v1" v1
        setVector app "v2" v2
        app.Execute("res = v1 .* v2;") |> ignore
        let res = getVector app "res"
        let v1 = new Vector(v1)
        let v2 = new Vector(v2)
        (v1 .* v2).ToArray() <=> res


    [<Property>]
    let ``X .* a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((X .* a).ToArray() <=> (X .* A).ToArray()) .&. ((X .* a).ToArray() <=> (x |> Array.map (fun z -> z * a)))

    [<Property>]
    let ``X * a`` (x : float[]) (a : float) =
        let X = new Vector(x)
        ((X * a).ToArray() <=> (x |> Array.map (fun z -> z * a)))

    [<Property>]
    let ``a .* X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((a .* X).ToArray() <=> (A .* X).ToArray()) .&. ((a .* X).ToArray() <=> (x |> Array.map (fun z -> a * z)))

    [<Property>]
    let ``a * X`` (x : float[]) (a : float) =
        let X = new Vector(x)
        ((a * X).ToArray() <=> (x |> Array.map (fun z -> a * z)))
    
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X ./ Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X ./ Y))

    [<Property>]
    let ``Operator ./``(v : (float*float)[]) =
        let v1 = v |> Array.map fst
        let v2 = v |> Array.map snd 
        let app = getApp()
        setVector app "v1" v1
        setVector app "v2" v2
        app.Execute("res = v1 ./ v2;") |> ignore
        let res = getVector app "res"
        let v1 = new Vector(v1)
        let v2 = new Vector(v2)
        epsEqual 1e-14 ((v1 ./ v2).ToArray()) res


    [<Property>]
    let ``X ./ a`` (x : float[]) (a : float) =
        let x = x |> Array.map regDouble
        let a = regDouble a
        let X = new Vector(x)
        let A = new Vector(a)
        (epsEqual 1e-14 ((X ./ a).ToArray()) ((X ./ A).ToArray())) .&. (epsEqual 1e-14 ((X ./ a).ToArray()) (x |> Array.map (fun z -> z / a)))

    [<Property>]
    let ``X / a`` (x : float[]) (a : float) =
        let x = x |> Array.map regDouble
        let a = regDouble a
        let X = new Vector(x)
        (epsEqual 1e-14 ((X / a).ToArray()) (x |> Array.map (fun z -> z / a)))

    [<Property>]
    let ``a ./ X`` (x : float[]) (a : float) =
        let x = x |> Array.map regDouble
        let a = regDouble a
        let X = new Vector(x)
        let A = new Vector(a)
        (epsEqual 1e-13 ((a ./ X).ToArray()) ((A ./ X).ToArray())) .&. (epsEqual 1e-13 ((a ./ X).ToArray()) (x |> Array.map (fun z -> a / z)))

    [<Property>]
    let ``a / X`` (x : float[]) (a : float) =
        let x = x |> Array.map regDouble
        let a = regDouble a
        let X = new Vector(x)
        (epsEqual 1e-13 ((a / X).ToArray()) (x |> Array.map (fun z -> a / z)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .^ Y`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .^ Y))

    [<Property>]
    let ``Operator .^``(v : (float*float)[]) =
        let v1 = v |> Array.map (fst>>abs) |> Array.map (fun x -> if Double.IsNaN(x) || Double.IsInfinity(x) then 0.0 else x)
        let v2 = v |> Array.map snd |> Array.map (fun x -> if Double.IsNaN(x) || Double.IsInfinity(x) then 0.0 else x)
        let app = getApp()
        setVector app "v1" v1
        setVector app "v2" v2
        app.Execute("res = v1 .^ v2;") |> ignore
        let res = new Vector(getVector app "res")
        let v1 = new Vector(v1)
        let v2 = new Vector(v2)
        let v3 = v1 .^ v2
        epsEqual 1e-14 (v3.ToArray()) (res.ToArray())

    [<Property>]
    let ``X .^ a`` (x : float[]) (a : float) =
        let x = x |> Array.map abs |> Array.filter (Double.IsNaN>>not)
        let a = if Double.IsNaN(a) then 1.0 else a
        let X = new Vector(x)
        let A = new Vector(a)
        (epsEqual 1e-14 ((X .^ a).ToArray()) ((X .^ A).ToArray())) .&. (epsEqual 1e-15 ((X .^ a).ToArray()) (x |> Array.map (fun z -> if a = 0.0 then 1.0 elif z = 1.0 then 1.0 else z ** a)))

    [<Property>]
    let ``X .^ n`` (x : float[]) (n : int) =
        let a = float n
        let x = x |> Array.map abs |> Array.filter (Double.IsNaN>>not)
        let a = if Double.IsNaN(a) then 1.0 else a
        let X = new Vector(x)
        let A = new Vector(a)
        (epsEqual 1e-15 ((X .^ n).ToArray()) (x |> Array.map (fun z -> if a = 0.0 then 1.0 elif z = 1.0 then 1.0 else z ** a)))

    [<Property>]
    let ``a .^ X`` (x : float[]) (a : float) =
        let a = if Double.IsNaN(a) then 1.0 else abs a
        let x = x |> Array.filter (Double.IsNaN>>not)
        let X = new Vector(x)
        let A = new Vector(a)
        (epsEqual 1e-14 ((a .^ X).ToArray()) ((A .^ X).ToArray())) .&. (epsEqual 1e-15 ((a .^ X).ToArray()) (x |> Array.map (fun z -> if z = 0.0 then 1.0 elif a = 1.0 then 1.0 else a ** z)))

    [<Property>]
    let ``Operator unary -``(v : float[]) = 
        let app = getApp()
        setVector app "v" v
        app.Execute("res = -v;") |> ignore
        let res = getVector app "res"
        let V = new Vector(v)
        (-V).ToArray() <=> res

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in Min(X, Y)`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(Vector.Min(X, Y)))

    [<Property>]
    let ``Min(X, Y)`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (Vector.Min(X, Y)).ToArray() <=> (y |> Array.zip x |> Array.map Math.Min)

    [<Property>]
    let ``Min(X, a)`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((Vector.Min(X, a).ToArray()) <=> (Vector.Min(X, A).ToArray())) .&. ((Vector.Min(X, a)).ToArray() <=> (x |> Array.map (fun z -> min z a)))

    [<Property>]
    let ``Min(a, X)`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((Vector.Min(a, X).ToArray()) <=> (Vector.Min(A, X).ToArray())) .&. ((Vector.Min(a, X)).ToArray() <=> (x |> Array.map (fun z -> min a z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in Max(X, Y)`` (x : float[]) (y : float[]) =
        let X = new Vector(x)
        let Y = new Vector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(Vector.Max(X, Y)))

    [<Property>]
    let ``Max(X, Y)`` (v : (float*float)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new Vector(x)
        let Y = new Vector(y)
        (Vector.Max(X, Y)).ToArray() <=> (y |> Array.zip x |> Array.map Math.Max) 

    [<Property>]
    let ``Max(X, a)`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((Vector.Max(X, a).ToArray()) <=> (Vector.Max(X, A).ToArray())) .&. ((Vector.Max(X, a)).ToArray() <=> (x |> Array.map (fun z -> max z a)))

    [<Property>]
    let ``Max(a, X)`` (x : float[]) (a : float) =
        let X = new Vector(x)
        let A = new Vector(a)
        ((Vector.Max(a, X).ToArray()) <=> (Vector.Max(A, X).ToArray())) .&. ((Vector.Max(a, X)).ToArray() <=> (x |> Array.map (fun z -> max a z)))