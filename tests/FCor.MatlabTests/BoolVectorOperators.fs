namespace FCor.MatlabTests

open FCor
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolVectorOperators = 

    [<Property>]
    let ``X == X`` (x : bool[]) =
        let X = new BoolVector(x)
        (X == X) .&. (X == BoolVector.Copy(X))

    [<Property>]
    let ``X = X`` (x : bool[]) =
        let X = new BoolVector(x)
        (X = X) .&. (X = BoolVector.Copy(X))

    [<Property(MaxTest=1000)>]
    let ``X != Y if not same length and data`` (x : bool[]) (y :  bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        x <> y ==> (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X <> Y if not same length and data`` (x : bool[]) (y :  bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        x <> y ==> (X <> Y)

    [<Property(MaxTest=1000)>]
    let ``X == Y = not X != Y`` (x : bool[]) (y :  bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X == Y) = not (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X = Y = not X <> Y`` (x : bool[]) (y :  bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X = Y) = not (X <> Y)

    [<Property>]
    let ``BoolVector(a) == a`` (a : bool) =
        let A = new BoolVector(a)
        (A == a) .&. (a == A)

    [<Property(MaxTest=1000)>]
    let ``X == a = not X != a`` (x : bool[]) (a :  bool) =
        let X = new BoolVector(x)
        (X == a) = not (X != a)

    [<Property(MaxTest=1000)>]
    let ``a == X = not a != X`` (x : bool[]) (a :  bool) =
        let X = new BoolVector(x)
        (a == X) = not (a != X)



    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .< Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .< Y))

    [<Property>]
    let ``X .< Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .< Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 < z2)) 

    [<Property>]
    let ``X .< a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .< a) == (X .< A)) .&. ((X .< a).ToArray() = (x |> Array.map (fun z -> z < a)))

    [<Property>]
    let ``a .< X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .< X) == (A .< X)) .&. ((a .< X).ToArray() = (x |> Array.map (fun z -> a < z)))



    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .<= Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .<= Y))

    [<Property>]
    let ``X .<= Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .<= Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 <= z2)) 

    [<Property>]
    let ``X .<= a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .<= a) == (X .<= A)) .&. ((X .<= a).ToArray() = (x |> Array.map (fun z -> z <= a)))

    [<Property>]
    let ``a .<= X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .<= X) == (A .<= X)) .&. ((a .<= X).ToArray() = (x |> Array.map (fun z -> a <= z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .> Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .> Y))

    [<Property>]
    let ``X .> Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .> Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 > z2)) 

    [<Property>]
    let ``X .> a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .> a) == (X .> A)) .&. ((X .> a).ToArray() = (x |> Array.map (fun z -> z > a)))

    [<Property>]
    let ``a .> X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .> X) == (A .> X)) .&. ((a .> X).ToArray() = (x |> Array.map (fun z -> a > z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .>= Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .>= Y))

    [<Property>]
    let ``X .>= Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .>= Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 >= z2)) 

    [<Property>]
    let ``X .>= a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .>= a) == (X .>= A)) .&. ((X .>= a).ToArray() = (x |> Array.map (fun z -> z >= a)))

    [<Property>]
    let ``a .>= X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .>= X) == (A .>= X)) .&. ((a .>= X).ToArray() = (x |> Array.map (fun z -> a >= z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .= Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .= Y))

    [<Property>]
    let ``X .= Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .= Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 = z2)) 

    [<Property>]
    let ``X .= a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .= a) == (X .= A)) .&. ((X .= a).ToArray() = (x |> Array.map (fun z -> z = a)))

    [<Property>]
    let ``a .= X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .= X) == (A .= X)) .&. ((a .= X).ToArray() = (x |> Array.map (fun z -> z = a)))
  
  
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .<> Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .<> Y))

    [<Property>]
    let ``X .<> Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .<> Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 <> z2)) 

    [<Property>]
    let ``X .<> a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .<> a) == (X .<> A)) .&. ((X .<> a).ToArray() = (x |> Array.map (fun z -> z <> a)))

    [<Property>]
    let ``a .<> X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .<> X) == (A .<> X)) .&. ((a .<> X).ToArray() = (x |> Array.map (fun z -> z <> a)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in Min(X, Y)`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(BoolVector.Min(X, Y)))

    [<Property>]
    let ``Min(X, Y)`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (BoolVector.Min(X, Y)).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> min z1 z2)) 

    [<Property>]
    let ``Min(X, a)`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((BoolVector.Min(X, a)) == (BoolVector.Min(X, A))) .&. ((BoolVector.Min(X, a)).ToArray() = (x |> Array.map (fun z -> min z a)))

    [<Property>]
    let ``Min(a, X)`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((BoolVector.Min(a, X)) == (BoolVector.Min(A, X))) .&. ((BoolVector.Min(a, X)).ToArray() = (x |> Array.map (fun z -> min a z)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in Max(X, Y)`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(BoolVector.Max(X, Y)))

    [<Property>]
    let ``Max(X, Y)`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (BoolVector.Max(X, Y)).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> max z1 z2)) 

    [<Property>]
    let ``Max(X, a)`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((BoolVector.Max(X, a)) == (BoolVector.Max(X, A))) .&. ((BoolVector.Max(X, a)).ToArray() = (x |> Array.map (fun z -> max z a)))
        
    [<Property>]
    let ``Max(a, X)`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((BoolVector.Max(a, X)) == (BoolVector.Max(A, X))) .&. ((BoolVector.Max(a, X)).ToArray() = (x |> Array.map (fun z -> max z a)))
                         
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .&& Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .&& Y))

    [<Property>]
    let ``X .&& Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .&& Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 && z2)) 

    [<Property>]
    let ``X .&& a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .&& a) == (X .&& A)) .&. ((X .&& a).ToArray() = (x |> Array.map (fun z -> z && a)))

    [<Property>]
    let ``a .&& X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .&& X) == (A .&& X)) .&. ((a .&& X).ToArray() = (x |> Array.map (fun z -> a && z)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if length mismatch in X .|| Y`` (x : bool[]) (y : bool[]) =
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        let len1 = X.LongLength
        let len2 = Y.LongLength
        ((len1 <> len2) && (len1 <> 1L) && (len2 <> 1L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .|| Y))

    [<Property>]
    let ``X .|| Y`` (v : (bool*bool)[]) =
        let x = v |> Array.map fst
        let y = v |> Array.map snd
        let X = new BoolVector(x)
        let Y = new BoolVector(y)
        (X .|| Y).ToArray() = (y |> Array.zip x |> Array.map (fun (z1,z2) -> z1 || z2)) 

    [<Property>]
    let ``X .|| a`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((X .|| a) == (X .|| A)) .&. ((X .|| a).ToArray() = (x |> Array.map (fun z -> z || a)))

    [<Property>]
    let ``a .|| X`` (x : bool[]) (a : bool) =
        let X = new BoolVector(x)
        let A = new BoolVector(a)
        ((a .|| X) == (A .|| X)) .&. ((a .|| X).ToArray() = (x |> Array.map (fun z -> a || z)))

    [<Property>]
    let ``Not X`` (x : bool[]) =
        let X = new BoolVector(x)
        (BoolVector.Not(X)).ToArray() = (x |> Array.map not) 


