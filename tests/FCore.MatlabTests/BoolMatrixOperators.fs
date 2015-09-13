namespace FCore.MatlabTests

open FCore
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open Util

module BoolMatrixOperators = 

    let fixEmpty (a : bool[,]) =
        if a.Length = 0 then Array2D.create 0 0 false
        else a

    [<Property>]
    let ``X == X`` (x : bool[,]) =
        let X = new BoolMatrix(x)
        (X == X) .&. (X == BoolMatrix.Copy(X))

    [<Property>]
    let ``X = X`` (x : bool[,]) =
        let X = new BoolMatrix(x)
        (X = X) .&. (X = BoolMatrix.Copy(X))

    [<Property(MaxTest=1000)>]
    let ``X != Y if not same shape and data`` (x : bool[,]) (y :  bool[,]) =
        let x = x |> fixEmpty
        let y = y |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        x <> y ==> (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X <> Y if not same shape and data`` (x : bool[,]) (y :  bool[,]) =
        let x = x |> fixEmpty
        let y = y |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        x <> y ==> (X <> Y)

    [<Property(MaxTest=1000)>]
    let ``X == Y = not X != Y`` (x : bool[,]) (y :  bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X == Y) = not (X != Y)

    [<Property(MaxTest=1000)>]
    let ``X = Y = not X <> Y`` (x : bool[,]) (y :  bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X = Y) = not (X <> Y)

    [<Property>]
    let ``BoolMatrix(a) == a`` (a : bool) =
        let A = new BoolMatrix(a)
        (A == a) .&. (a == A)

    [<Property(MaxTest=1000)>]
    let ``X == a = not X != a`` (x : bool[,]) (a :  bool) =
        let X = new BoolMatrix(x)
        (X == a) = not (X != a)

    [<Property(MaxTest=1000)>]
    let ``a == X = not a != X`` (x : bool[,]) (a :  bool) =
        let X = new BoolMatrix(x)
        (a == X) = not (a != X)



    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .< Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==>  
            Prop.throws<ArgumentException, _> (lazy(X .< Y))

    [<Property>]
    let ``X .< Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .< Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 < z2)) 

    [<Property>]
    let ``X .< a`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .< a) == (X .< A)) .&. ((X .< a).ToArray2D() = (x |> Array2D.map (fun z -> z < a)))

    [<Property>]
    let ``a .< X`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .< X) == (A .< X)) .&. ((a .< X).ToArray2D() = (x |> Array2D.map (fun z -> a < z)))



    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .<= Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==>  
            Prop.throws<ArgumentException, _> (lazy(X .<= Y))

    [<Property>]
    let ``X .<= Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .<= Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 <= z2)) 

    [<Property>]
    let ``X .<= a`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .<= a) == (X .<= A)) .&. ((X .<= a).ToArray2D() = (x |> Array2D.map (fun z -> z <= a)))

    [<Property>]
    let ``a .<= X`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .<= X) == (A .<= X)) .&. ((a .<= X).ToArray2D() = (x |> Array2D.map (fun z -> a <= z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .> Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==>  
            Prop.throws<ArgumentException, _> (lazy(X .> Y))

    [<Property>]
    let ``X .> Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .> Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 > z2)) 

    [<Property>]
    let ``X .> a`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .> a) == (X .> A)) .&. ((X .> a).ToArray2D() = (x |> Array2D.map (fun z -> z > a)))

    [<Property>]
    let ``a .> X`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .> X) == (A .> X)) .&. ((a .> X).ToArray2D() = (x |> Array2D.map (fun z -> a > z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .>= Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .>= Y))

    [<Property>]
    let ``X .>= Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .>= Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 >= z2)) 

    [<Property>]
    let ``X .>= a`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .>= a) == (X .>= A)) .&. ((X .>= a).ToArray2D() = (x |> Array2D.map (fun z -> z >= a)))

    [<Property>]
    let ``a .>= X`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .>= X) == (A .>= X)) .&. ((a .>= X).ToArray2D() = (x |> Array2D.map (fun z -> a >= z)))


    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .= Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==>  
            Prop.throws<ArgumentException, _> (lazy(X .= Y))

    [<Property>]
    let ``X .= Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .= Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 = z2)) 

    [<Property>]
    let ``X .= a`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .= a) == (X .= A)) .&. ((X .= a).ToArray2D() = (x |> Array2D.map (fun z -> z = a)))

    [<Property>]
    let ``a .= X`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .= X) == (A .= X)) .&. ((a .= X).ToArray2D() = (x |> Array2D.map (fun z -> z = a)))
  
  
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .<> Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==>  
            Prop.throws<ArgumentException, _> (lazy(X .<> Y))

    [<Property>]
    let ``X .<> Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .<> Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 <> z2)) 

    [<Property>]
    let ``X .<> a`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .<> a) == (X .<> A)) .&. ((X .<> a).ToArray2D() = (x |> Array2D.map (fun z -> z <> a)))

    [<Property>]
    let ``a .<> X`` (x : bool[,]) (a : bool) =
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .<> X) == (A .<> X)) .&. ((a .<> X).ToArray2D() = (x |> Array2D.map (fun z -> z <> a)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in Min(X, Y)`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(BoolMatrix.Min(X, Y)))

    [<Property>]
    let ``Min(X, Y)`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (BoolMatrix.Min(X, Y)).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> min z1 z2)) 

    [<Property>]
    let ``Min(X, a)`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((BoolMatrix.Min(X, a)) == (BoolMatrix.Min(X, A))) .&. ((BoolMatrix.Min(X, a)).ToArray2D() = (x |> Array2D.map (fun z -> min z a)))

    [<Property>]
    let ``Min(a, X)`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((BoolMatrix.Min(a, X)) == (BoolMatrix.Min(A, X))) .&. ((BoolMatrix.Min(a, X)).ToArray2D() = (x |> Array2D.map (fun z -> min a z)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in Max(X, Y)`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(BoolMatrix.Max(X, Y)))

    [<Property>]
    let ``Max(X, Y)`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (BoolMatrix.Max(X, Y)).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> max z1 z2)) 

    [<Property>]
    let ``Max(X, a)`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((BoolMatrix.Max(X, a)) == (BoolMatrix.Max(X, A))) .&. ((BoolMatrix.Max(X, a)).ToArray2D() = (x |> Array2D.map (fun z -> max z a)))
        
    [<Property>]
    let ``Max(a, X)`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((BoolMatrix.Max(a, X)) == (BoolMatrix.Max(A, X))) .&. ((BoolMatrix.Max(a, X)).ToArray2D() = (x |> Array2D.map (fun z -> max z a)))
                         
    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .&& Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==> 
            Prop.throws<ArgumentException, _> (lazy(X .&& Y))

    [<Property>]
    let ``X .&& Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .&& Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 && z2)) 

    [<Property>]
    let ``X .&& a`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .&& a) == (X .&& A)) .&. ((X .&& a).ToArray2D() = (x |> Array2D.map (fun z -> z && a)))

    [<Property>]
    let ``a .&& X`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .&& X) == (A .&& X)) .&. ((a .&& X).ToArray2D() = (x |> Array2D.map (fun z -> a && z)))

    [<Property(MaxTest=1000)>]
    let ``Throws arg exception if shape mismatch in X .|| Y`` (x : bool[,]) (y : bool[,]) =
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        let rows1 = X.LongRowCount
        let rows2 = Y.LongRowCount
        let cols1 = X.LongColCount
        let cols2 = Y.LongColCount
        (((rows1 <> rows2) || (cols1 <> cols2)) && (X.LongLength <> 1L) && (Y.LongLength <> 1L) && (X.LongLength <> 0L || Y.LongLength <> 0L)) ==>  
            Prop.throws<ArgumentException, _> (lazy(X .|| Y))

    [<Property>]
    let ``X .|| Y`` (v : (bool*bool)[,]) =
        let x = v |> Array2D.map fst |> fixEmpty
        let y = v |> Array2D.map snd |> fixEmpty
        let X = new BoolMatrix(x)
        let Y = new BoolMatrix(y)
        (X .|| Y).ToArray2D() = (y |> array2DZip x |> Array2D.map (fun (z1,z2) -> z1 || z2)) 

    [<Property>]
    let ``X .|| a`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((X .|| a) == (X .|| A)) .&. ((X .|| a).ToArray2D() = (x |> Array2D.map (fun z -> z || a)))

    [<Property>]
    let ``a .|| X`` (x : bool[,]) (a : bool) =
        let x = x |> fixEmpty
        let X = new BoolMatrix(x)
        let A = new BoolMatrix(a)
        ((a .|| X) == (A .|| X)) .&. ((a .|| X).ToArray2D() = (x |> Array2D.map (fun z -> a || z)))

    [<Property>]
    let ``Not X`` (x : bool[,]) =
        let X = new BoolMatrix(x)
        (BoolMatrix.Not(X)).ToArray2D() = (x |> Array2D.map not) 

