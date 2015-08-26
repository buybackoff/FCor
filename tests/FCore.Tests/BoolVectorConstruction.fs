namespace FCore.Tests

open FCore
open Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolVectorConstruction =

    [<Property>]
    let ``Constructs empty vector`` () =
        let v = BoolVector.Empty
        v.LongLength = 0L && v.ToArray() = Array.zeroCreate<bool> 0

    [<Property>]
    let ``Throws arg exception if construction with len64 < 0`` (len : int64) x =
        len < 0L ==> Prop.throws<ArgumentException, _> (lazy(new BoolVector(len, x)))

    [<Property>]
    let ``Throws arg exception if construction with len < 0`` (len : int) (x : bool) =
        len < 0 ==> Prop.throws<ArgumentException, _> (lazy(new BoolVector(len, x)))

    [<Property>]
    let ``Constructs BoolVector from non negative int64 and bool value`` (len : int64) x =
        len >= 0L ==> lazy(let v = new BoolVector(len, x) in v.LongLength = len && v.ToArray() = Array.create (int(len)) x)

    [<Property>]
    let ``Constructs BoolVector from non negative int and bool value`` (len : int) (x : bool) =
        len >= 0 ==> lazy(let v = new BoolVector(len, x) in v.Length = len && v.ToArray() = Array.create len x)

    [<Property>]
    let ``Constructs BoolVector from bool value`` (x : bool) =
        let v = new BoolVector(x) in v.Length = 1 && v.ToArray() = [|x|]

    [<Property>]
    let ``Constructs BoolVector from bool seq`` (data : bool[]) = 
        let s = data |> Array.toSeq
        let v = new BoolVector(s) in v.Length = data.Length && v.ToArray() = data

    [<Property>]
    let ``Constructs BoolVector from int and initializer function`` (len : int) (init : int -> bool) = 
        len >= 0 ==> lazy (let v = new BoolVector(len, init) in v.Length = len && v.ToArray() = Array.init len init)

    [<Property>]
    let ``Constructs BoolVector from bool array with copy`` (data : bool[]) = 
        let v = new BoolVector(data, true)
        if data.Length > 0 then
            data.[0] <- not data.[0]
            v.Length = data.Length && v.ToArray().[1..] = data.[1..] && v.ToArray().[0] <> data.[0]
        else
            v.Length = data.Length && v.ToArray() = data

    [<Property>]
    let ``Constructs BoolVector from bool array without copy`` (data : bool[]) = 
        let v = new BoolVector(data, false)
        if data.Length > 0 then
            data.[0] <- not data.[0]
        v.Length = data.Length && v.ToArray() = data




