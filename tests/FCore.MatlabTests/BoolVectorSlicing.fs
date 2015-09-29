namespace FCore.MatlabTests

open FCore
open FCore.Math
open FCore.BasicStats
open FCore.Random
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module BoolVectorSlicing =

    let app = new MLAppClass()
    do app.Visible <- 0

    let rnd = new Random()

    [<Property>]
    let ``GetItem int64``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let index = rnd.Next(v.Length) |> int64
                                    setScalar app "index" (float(index + 1L))
                                    app.Execute("res = v(index);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    [|v.[index]|] = res       
                                    )

    [<Property>]
    let ``GetItem int``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let index = rnd.Next(v.Length) 
                                    setScalar app "index" (float(index + 1))
                                    app.Execute("res = v(index);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    [|v.[index]|] = res       
                                    )

    [<Property>]
    let ``SetItem int64``(v : bool[]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let index = rnd.Next(v.Length) |> int64
                                    setScalar app "index" (float(index + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(index) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.[index] <- a
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem int``(v : bool[]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let index = rnd.Next(v.Length) 
                                    setScalar app "index" (float(index + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(index) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.[index] <- a
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem int64 seq``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("res = v(indices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[indices |> Array.toSeq].ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem int seq``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length))
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("res = v(indices);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[indices |> Array.toSeq].ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem int64 seq scalar``(v : bool[]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.[indices |> Array.toSeq] <- a
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem int seq scalar``(v : bool[]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "a" [|a|]
                                    let a = new BoolVector(a)
                                    setBoolVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length))
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.[indices |> Array.toSeq] <- a
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem int64 seq vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    let a = v |> Array.map (fun x -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.[indices |> Array.toSeq] <- new BoolVector(a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem int seq vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length))
                                    let a = v |> Array.map (fun x -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "a" a
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.[indices |> Array.toSeq] <- new BoolVector(a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int64, Some int64``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    app.Execute("res = v(fromIndex:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[fromIndex..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int64, None``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    app.Execute("res = v(fromIndex:end);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[fromIndex..].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice None, Some int64``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    app.Execute("res = v(1:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int, Some int``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length)
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    app.Execute("res = v(fromIndex:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[fromIndex..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice Some int, None``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length)
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    app.Execute("res = v(fromIndex:end);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[fromIndex..].ToArray() = res       
                                    )

    [<Property>]
    let ``GetSlice None, Some int``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    app.Execute("res = v(1:toIndex);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    v.[..toIndex].ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 scalar``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some(fromIndex), Some(toIndex), a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 scalar vector``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some(fromIndex), Some(toIndex), new BoolVector(a))
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None scalar``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None scalar vector``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, None, new BoolVector(a))
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 scalar``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 scalar vector``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(None, Some toIndex, new BoolVector(a))
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int scalar``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int scalar vector``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, new BoolVector(a))
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None scalar``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None scalar vector``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, None, new BoolVector(a))
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int scalar``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int scalar vector``(v : bool[]) (a:bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setBoolScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(None, Some toIndex, new BoolVector(a))
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    let a =
                                        if toIndex >= fromIndex then
                                            new BoolVector(Array.init (int(toIndex - fromIndex + 1L)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let v = new BoolVector(v)
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    let a : BoolVector =  new BoolVector(Array.init (int(v.LongLength - fromIndex)) (fun i -> rnd.NextDouble() < 0.5))
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    let a =
                                        if toIndex >= 0L then
                                            new BoolVector(Array.init (int(toIndex + 1L)) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    let a =
                                        if toIndex >= fromIndex then
                                            new BoolVector(Array.init (toIndex - fromIndex + 1) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let v = new BoolVector(v)
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    let a : BoolVector = new BoolVector(Array.init (v.Length - fromIndex) (fun i -> rnd.NextDouble() < 0.5))
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    v.SetSlice(Some fromIndex, None, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    let a =
                                        if toIndex >= 0 then
                                            new BoolVector(Array.init (toIndex + 1) (fun i -> rnd.NextDouble() < 0.5))
                                        else BoolVector.Empty
                                    setBoolVector app "a" (a.ToArray())
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``GetItem bool vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "b" b
                                    app.Execute("res = v(b);") |> ignore
                                    let res = getBoolVector app "res"
                                    let v = new BoolVector(v)
                                    let b = new BoolVector(b)
                                    v.[b].ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem bool vector``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> Array.filter id
                                    let y = trueB |> Array.map (fun x -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "b" b
                                    setBoolVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    let b = new BoolVector(b)
                                    v.[b] <- new BoolVector(y)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``SetItem bool scalar vector``(v : bool[]) (a : bool) =
        (v.LongLength > 0L) ==> lazy(
                                    setBoolVector app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> Array.filter id
                                    let y = [|a|]
                                    setBoolVector app "b" b
                                    setBoolVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getBoolVector app "v"
                                    let v = new BoolVector(v)
                                    let b = new BoolVector(b)
                                    v.[b] <- new BoolVector(y)
                                    v.ToArray() = res       
                                    )

    [<Property>]
    let ``View int64``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    let v = new BoolVector(v)
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let length = rnd.Next(1, v.Length - int(fromIndex) + 1) |> int64
                                    let view = v.View(fromIndex, fromIndex + length - 1L)                    
                                    let y = new BoolVector(Array.init (int(length)) (fun i -> rnd.NextDouble() < 0.5))
                                    v.SetSlice(Some fromIndex, Some (fromIndex + length - 1L), y)
                                    view.ToArray() = v.[fromIndex..(fromIndex + length - 1L)].ToArray() && view.IsView
                                    )

    [<Property>]
    let ``View int``(v : bool[]) =
        (v.LongLength > 0L) ==> lazy(
                                    let v = new BoolVector(v)
                                    let fromIndex = rnd.Next(v.Length)
                                    let length = rnd.Next(1, v.Length - fromIndex + 1)
                                    let view = v.View(fromIndex, fromIndex + length - 1)                    
                                    let y = new BoolVector(Array.init length (fun i -> rnd.NextDouble() < 0.5))
                                    v.SetSlice(Some fromIndex, Some (fromIndex + length - 1), y)
                                    view.ToArray() = v.[fromIndex..(fromIndex + length - 1)].ToArray() && view.IsView
                                    )

