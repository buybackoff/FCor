namespace FCor.Tests

open FCor
open FCor.Math
open Xunit
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit
open System

module BoolMatrixConstruction =

    [<Property>]
    let ``Constructs empty matrix`` () =
        let v = BoolMatrix.Empty
        v.LongRowCount = 0L && v.LongColCount = 0L && v.ToArray2D() = Array2D.zeroCreate<bool> 0 0

    [<Property>]
    let ``Throws arg exception if construction with row64 or col64 < 0`` (r : int64) (c : int64) (x:bool) =
        (r < 0L || c < 0L) ==> Prop.throws<ArgumentException, _> (lazy(new BoolMatrix(r, c, x)))

    [<Property>]
    let ``Throws arg exception if construction with row or col < 0`` (r : int) (c: int) (x : bool) =
        (r < 0 || c < 0) ==> Prop.throws<ArgumentException, _> (lazy(new BoolMatrix(r, c, x)))

    [<Property>]
    let ``Constructs BoolMatrix from positive int64s and bool value`` (r : int64) (c : int64) (x:bool) =
        (r > 0L && c > 0L && r * c < 1000000L) ==> lazy(let v = new BoolMatrix(r, c, x) in v.LongRowCount = r && v.LongColCount = c && v.ToArray2D() = Array2D.create (int(r)) (int(c)) x)

    [<Property>]
    let ``Constructs empty BoolMatrix from r > 0L and c = 0L and bool value`` (r : int64) (x:bool) =
        let c = 0L
        (r > 0L) ==> lazy(let v = new BoolMatrix(r, c, x) in v.LongRowCount = 0L && v.LongColCount = 0L && v.ToArray2D() = Array2D.create 0 0 x)

    [<Property>]
    let ``Constructs empty BoolMatrix from c > 0L and r = 0L and bool value`` (c : int64) (x:bool) =
        let r = 0L
        (c > 0L) ==> lazy(let v = new BoolMatrix(r, c, x) in v.LongRowCount = 0L && v.LongColCount = 0L && v.ToArray2D() = Array2D.create 0 0 x)

    [<Property>]
    let ``Constructs BoolMatrix from positive ints and bool value`` (r : int) (c : int) (x:bool) =
        (r > 0 && c > 0 && r * c < 1000000) ==> lazy(let v = new BoolMatrix(r, c, x) in v.RowCount = r && v.ColCount = c && v.ToArray2D() = Array2D.create r c x)

    [<Property>]
    let ``Constructs empty BoolMatrix from r > 0 and c = 0 and bool value`` (r : int) (x:bool) =
        let c = 0
        (r > 0) ==> lazy(let v = new BoolMatrix(r, c, x) in v.RowCount = 0 && v.ColCount = 0 && v.ToArray2D() = Array2D.create 0 0 x)

    [<Property>]
    let ``Constructs empty BoolMatrix from c > 0 and r = 0 and bool value`` (c : int) (x:bool) =
        let r = 0
        (c > 0) ==> lazy(let v = new BoolMatrix(r, c, x) in v.RowCount = 0 && v.ColCount = 0 && v.ToArray2D() = Array2D.create 0 0 x)

    [<Property>]
    let ``Constructs BoolMatrix from bool value`` (x : bool) =
        let v = new BoolMatrix(x) in v.RowCount = 1 && v.ColCount = 1 && v.ToArray2D() = ([[x]]|>array2D)

    [<Property>]
    let ``Constructs BoolMatrix from bool array2d`` (data : bool[,]) = 
        let data = data |> Util.fixEmpty
        let v = new BoolMatrix(data) in v.RowCount = (data|>Array2D.length1) && v.ColCount = (data|>Array2D.length2) && v.ToArray2D() = data

    [<Property>]
    let ``Constructs BoolMatrix from ints and initializer function`` (r : int) (c : int) (init : int -> int -> bool) = 
        (r > 0 && c > 0) ==> lazy (let v = new BoolMatrix(r, c, init) in v.RowCount = r && v.ColCount = c && v.ToArray2D() = Array2D.init r c init)

    [<Property>]
    let ``Copy BoolMatrix`` (data : bool[,]) = 
        let v = new BoolMatrix(data)
        let s = BoolMatrix.Copy(v)
        s = v && (s.ColMajorDataVector.NativeArray <> v.ColMajorDataVector.NativeArray || (s.LongLength = 0L && v.LongLength = 0L))

    [<Property>]
    let ``Identity BoolMatrix int64`` (data : bool[,]) =
        let x : BoolMatrix = I (data.GetLength(0) |> int64) (data.GetLength(1) |> int64) 
        seq{for i in 0..data.GetLength(0)-1 do
                for j in 0..data.GetLength(1)-1 do
                    yield
                        if i = j then x.[i, j]
                        else not x.[i, j]
           } |> Seq.fold (&&) true

    [<Property>]
    let ``Identity BoolMatrix int`` (data : bool[,]) =
        let x : BoolMatrix = I (data.GetLength(0)) (data.GetLength(1)) 
        seq{for i in 0..data.GetLength(0)-1 do
                for j in 0..data.GetLength(1)-1 do
                    yield
                        if i = j then x.[i, j]
                        else not x.[i, j]
           } |> Seq.fold (&&) true
