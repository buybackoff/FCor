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

module VectorSlicing =

    let app = new MLAppClass()
    do app.Visible <- 0

    let rnd = new Random()

    let rng = new MT19937Rng()

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[]) (y :float[])  = epsEqualArray x y epsEqualFloat eps

    [<Property>]
    let ``GetItem int64``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let index = rnd.Next(v.Length) |> int64
                                    setScalar app "index" (float(index + 1L))
                                    app.Execute("res = v(index);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 ([|v.[index]|]) res       
                                    )

    [<Property>]
    let ``GetItem int``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let index = rnd.Next(v.Length) 
                                    setScalar app "index" (float(index + 1))
                                    app.Execute("res = v(index);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 ([|v.[index]|]) res       
                                    )

    [<Property>]
    let ``SetItem int64``(v : float[]) (a : float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let index = rnd.Next(v.Length) |> int64
                                    setScalar app "index" (float(index + 1L))
                                    setScalar app "a" a
                                    app.Execute("v(index) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.[index] <- a
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetItem int``(v : float[]) (a : float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let index = rnd.Next(v.Length) 
                                    setScalar app "index" (float(index + 1))
                                    setScalar app "a" a
                                    app.Execute("v(index) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.[index] <- a
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``GetItem int64 seq``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("res = v(indices);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[indices |> Array.toSeq].ToArray()) res       
                                    )

    [<Property>]
    let ``GetItem int seq``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length))
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("res = v(indices);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[indices |> Array.toSeq].ToArray()) res       
                                    )

    [<Property>]
    let ``SetItem int64 seq scalar``(v : float[]) (a : float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "a" [|a|]
                                    let a = new Vector(a)
                                    setVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.[indices |> Array.toSeq] <- a
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetItem int seq scalar``(v : float[]) (a : float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "a" [|a|]
                                    let a = new Vector(a)
                                    setVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length))
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.[indices |> Array.toSeq] <- a
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetItem int64 seq vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length) |> int64)
                                    let a = v |> Array.map (fun x -> rnd.NextDouble())
                                    setVector app "a" a
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1L)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.[indices |> Array.toSeq] <- new Vector(a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetItem int seq vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let indices = v |> Array.map (fun x -> rnd.Next(v.Length))
                                    let a = v |> Array.map (fun x -> rnd.NextDouble())
                                    setVector app "a" a
                                    setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
                                    app.Execute("v(indices) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.[indices |> Array.toSeq] <- new Vector(a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``GetSlice Some int64, Some int64``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    app.Execute("res = v(fromIndex:toIndex);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[fromIndex..toIndex].ToArray()) res       
                                    )

    [<Property>]
    let ``GetSlice Some int64, None``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    app.Execute("res = v(fromIndex:end);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[fromIndex..].ToArray()) res       
                                    )

    [<Property>]
    let ``GetSlice None, Some int64``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    app.Execute("res = v(1:toIndex);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[..toIndex].ToArray()) res       
                                    )

    [<Property>]
    let ``GetSlice Some int, Some int``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length)
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    app.Execute("res = v(fromIndex:toIndex);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[fromIndex..toIndex].ToArray()) res       
                                    )

    [<Property>]
    let ``GetSlice Some int, None``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length)
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    app.Execute("res = v(fromIndex:end);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[fromIndex..].ToArray()) res       
                                    )

    [<Property>]
    let ``GetSlice None, Some int``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    app.Execute("res = v(1:toIndex);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    epsEqual 0.0 (v.[..toIndex].ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 scalar``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 scalar vector``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, new Vector(a))
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None scalar``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, None, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None scalar vector``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, None, new Vector(a))
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 scalar``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 scalar vector``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    setScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(None, Some toIndex, new Vector(a))
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int scalar``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int scalar vector``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, new Vector(a))
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None scalar``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, None, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None scalar vector``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "a" a
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, None, new Vector(a))
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int scalar``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int scalar vector``(v : float[]) (a:float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    setScalar app "a" a
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(None, Some toIndex, new Vector(a))
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, Some int64 vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    let a =
                                        if toIndex >= fromIndex then
                                            rand rng (toIndex - fromIndex + 1L)
                                        else Vector.Empty
                                    setVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int64, None vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let v = new Vector(v)
                                    setScalar app "fromIndex" (float(fromIndex + 1L))
                                    let a : Vector =  rand rng (v.LongLength - fromIndex)
                                    setVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getVector app "v"
                                    v.SetSlice(Some fromIndex, None, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int64 vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) |> int64
                                    setScalar app "toIndex" (float(toIndex + 1L))
                                    let a =
                                        if toIndex >= 0L then
                                            rand rng (toIndex + 1L)
                                        else Vector.Empty
                                    setVector app "a" (a.ToArray())
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int, Some int vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    let a =
                                        if toIndex >= fromIndex then
                                            rand rng (toIndex - fromIndex + 1)
                                        else Vector.Empty
                                    setVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(Some fromIndex, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice Some int, None vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let fromIndex = rnd.Next(v.Length) 
                                    let v = new Vector(v)
                                    setScalar app "fromIndex" (float(fromIndex + 1))
                                    let a : Vector =  rand rng (v.Length - fromIndex)
                                    setVector app "a" (a.ToArray())
                                    app.Execute("v(fromIndex:end) = a;") |> ignore
                                    let res = getVector app "v"
                                    v.SetSlice(Some fromIndex, None, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetSlice None, Some int vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let toIndex = rnd.Next(v.Length) 
                                    setScalar app "toIndex" (float(toIndex + 1))
                                    let a =
                                        if toIndex >= 0 then
                                            rand rng (toIndex + 1)
                                        else Vector.Empty
                                    setVector app "a" (a.ToArray())
                                    app.Execute("v(1:toIndex) = a;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    v.SetSlice(None, Some toIndex, a)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``GetItem bool vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    setBoolVector app "b" b
                                    app.Execute("res = v(b);") |> ignore
                                    let res = getVector app "res"
                                    let v = new Vector(v)
                                    let b = new BoolVector(b)
                                    epsEqual 0.0 (v.[b].ToArray()) res       
                                    )

    [<Property>]
    let ``SetItem bool vector``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> Array.filter id
                                    let y = trueB |> Array.map (fun x -> rnd.NextDouble())
                                    setBoolVector app "b" b
                                    setVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    let b = new BoolVector(b)
                                    v.[b] <- new Vector(y)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``SetItem bool scalar vector``(v : float[]) (a : float) =
        (v.LongLength > 0L) ==> lazy(
                                    setVector app "v" v
                                    let b = Array.init v.Length (fun i -> rnd.NextDouble() < 0.5)
                                    let trueB = b |> Array.filter id
                                    let y = [|a|]
                                    setBoolVector app "b" b
                                    setVector app "y" y
                                    app.Execute("v(b) = y;") |> ignore
                                    let res = getVector app "v"
                                    let v = new Vector(v)
                                    let b = new BoolVector(b)
                                    v.[b] <- new Vector(y)
                                    epsEqual 0.0 (v.ToArray()) res       
                                    )

    [<Property>]
    let ``View int64``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    let v = new Vector(v)
                                    let fromIndex = rnd.Next(v.Length) |> int64
                                    let length = rnd.Next(1, v.Length - int(fromIndex) + 1) |> int64
                                    let view = v.View(fromIndex, fromIndex + length - 1L)                    
                                    let y = new Vector(Array.init (int(length)) (fun i -> rnd.NextDouble()))
                                    v.SetSlice(Some fromIndex, Some (fromIndex + length - 1L), y)
                                    view.ToArray() = v.[fromIndex..(fromIndex + length - 1L)].ToArray() && view.IsView
                                    )

    [<Property>]
    let ``View int``(v : float[]) =
        (v.LongLength > 0L) ==> lazy(
                                    let v = new Vector(v)
                                    let fromIndex = rnd.Next(v.Length)
                                    let length = rnd.Next(1, v.Length - fromIndex + 1)
                                    let view = v.View(fromIndex, fromIndex + length - 1)                    
                                    let y = new Vector(Array.init length (fun i -> rnd.NextDouble()))
                                    v.SetSlice(Some fromIndex, Some (fromIndex + length - 1), y)
                                    view.ToArray() = v.[fromIndex..(fromIndex + length - 1)].ToArray() && view.IsView
                                    )


