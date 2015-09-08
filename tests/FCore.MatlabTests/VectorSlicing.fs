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


