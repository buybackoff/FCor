namespace FCore.Tests

open FCore
open Xunit
open FsCheck
open FsCheck.Xunit
open System

module VectorConstruction =
    //needed as fscheck behaves differently if finds nan in array
    let cln x = if System.Double.IsInfinity(x)||System.Double.IsNaN(x) then 0.0 else x

    [<Property>]
    let ``Constructs empty vector`` () =
        let v = Vector.Empty
        v.LongLength = 0L && v.ToArray() = Array.zeroCreate<float> 0

    [<Property>]
    let ``Throws arg exception if construction with len64 < 0`` (len : int64) ix =
        let x = cln ix
        len < 0L ==> Prop.throws<ArgumentException, _> (lazy(new Vector(len, x)))

    [<Property>]
    let ``Throws arg exception if construction with len < 0`` (len : int) (ix : float) =
        let x = cln ix
        len < 0 ==> Prop.throws<ArgumentException, _> (lazy(new Vector(len, x)))

    [<Property>]
    let ``Constructs Vector from non negative int64 and float value`` (len : int64) ix =
        let x = cln ix
        len >= 0L ==> lazy(let v = new Vector(len, x) in v.LongLength = len && v.ToArray() = Array.create (int(len)) x)

    [<Property>]
    let ``Constructs Vector from non negative int and float value`` (len : int) (ix : float) =
        let x = cln ix
        len >= 0 ==> lazy(let v = new Vector(len, x) in v.Length = len && v.ToArray() = Array.create len x)

    [<Property>]
    let ``Constructs Vector from float value`` (ix : float) =
        let x = cln ix
        let v = new Vector(x) in v.Length = 1 && v.ToArray() = [|x|]

    [<Property>]
    let ``Constructs Vector from float seq`` (idata : float[]) = 
        let data=idata|>Array.map cln
        let s = data |> Array.toSeq
        let v = new Vector(s) in v.Length = data.Length && v.ToArray() = data

    [<Property>]
    let ``Constructs Vector from int and initializer function`` (len : int) (init : int -> float) = 
        len >= 0 ==> lazy (let v = new Vector(len, init>>cln) in v.Length = len && v.ToArray() = Array.init len (init>>cln))

    [<Property>]
    let ``Constructs Vector from float array with copy`` (idata : float[]) = 
        let data=idata|>Array.map cln
        let v = new Vector(data, true)
        if data.Length > 0 then
            data.[0] <- 0.12341234
            v.Length = data.Length && v.ToArray().[1..] = data.[1..] && v.ToArray().[0] <> data.[0]
        else
            v.Length = data.Length && v.ToArray() = data

    [<Property>]
    let ``Constructs Vector from float array without copy`` (idata : float[]) = 
        let data=idata|>Array.map cln
        let v = new Vector(data, false)
        if data.Length > 0 then
            data.[0] <- - data.[0]
        v.Length = data.Length && v.ToArray() = data




