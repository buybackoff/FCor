namespace FCor.Vector.Tests

open FCor
open Xunit
open FsCheck
open FsCheck.Xunit
open System
open Util

module VectorConstruction =

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    [<Property>]
    let ``Constructs empty vector`` () =
        let v = Vector.Empty
        v.LongLength = 0L && v.ToArray() = Array.zeroCreate<float> 0

    [<Property>]
    let ``Throws arg exception if construction with len64 < 0`` (len : int64) x =
        len < 0L ==> Prop.throws<ArgumentException, _> (lazy(new Vector(len, x)))

    [<Property>]
    let ``Throws arg exception if construction with len < 0`` (len : int) (x : float) =
        len < 0 ==> Prop.throws<ArgumentException, _> (lazy(new Vector(len, x)))

    [<Property>]
    let ``Constructs Vector from non negative int64 and float value`` (len : int64) x =
        len >= 0L ==> lazy(let v = new Vector(len, x) in v.LongLength = len && v.ToArray() <=> Array.create (int(len)) x)

    [<Property>]
    let ``Constructs Vector from non negative int and float value`` (len : int) (x : float) =
        len >= 0 ==> lazy(let v = new Vector(len, x) in v.Length = len && v.ToArray() <=> Array.create len x)

    [<Property>]
    let ``Constructs Vector from float value`` (x : float) =
        let v = new Vector(x) in v.Length = 1 && v.ToArray() <=> [|x|]

    [<Property>]
    let ``Constructs Vector from float seq`` (data : float[]) = 
        let s = data |> Array.toSeq
        let v = new Vector(s) in v.Length = data.Length && v.ToArray() <=> data

    [<Property>]
    let ``Constructs Vector from int and initializer function`` (len : int) (init : int -> float) = 
        len >= 0 ==> lazy (let v = new Vector(len, init) in v.Length = len && v.ToArray() <=> Array.init len init)

    [<Property>]
    let ``Constructs Vector from float array with copy`` (data : float[]) = 
        let v = new Vector(data, true)
        if data.Length > 0 && data.[0] <> 0.0 then
            data.[0] <- -data.[0]
            v.Length = data.Length && v.ToArray().[1..] <=> data.[1..] && v.ToArray().[0] <> data.[0]
        else
            v.Length = data.Length && v.ToArray() <=> data

    [<Property>]
    let ``Constructs Vector from float array without copy`` (data : float[]) = 
        let v = new Vector(data, false)
        if data.Length > 0 then
            data.[0] <- - data.[0]
        v.Length = data.Length && v.ToArray() <=> data

    [<Property>]
    let ``Copy Vector`` (data : float[]) = 
        let v = new Vector(data)
        let s = Vector.Copy(v)
        s.ToArray() <=> v.ToArray() && (s.NativeArray <> v.NativeArray || (s.LongLength = 0L && v.LongLength = 0L))

    [<Property>]
    let ``Concat Vector seq`` (data : float[][]) =
        let vectors = data |> Seq.map (fun x -> new Vector(x))
        let res = Vector.Concat vectors
        let vectors = vectors |> Seq.filter (fun v -> v.LongLength <> 0L)
        let startIndexSeq = vectors |> Seq.scan (fun index v -> index + v.LongLength) 0L
        vectors |> Seq.zip startIndexSeq |> Seq.map (fun (startIndex, v) -> res.[startIndex..startIndex + v.LongLength - 1L].ToArray() <=> v.ToArray()) |> Seq.fold (&&) true 




