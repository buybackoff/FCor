module FCore.Tests

open FCore
open Util
open Xunit
open FsCheck
open FsCheck.Xunit
open System

[<Property>]
let ``Constructs BoolVector from int64 and bool value`` (len : int64) x = 
    len >= 0L ==> lazy ((new BoolVector(len, x)).LongLength = len)

[<Property>]
let ``Constructs BoolVector from int and bool value`` (len : int) (x : bool) = 
    len >= 0 ==> lazy ((new BoolVector(len, x)).Length = len)

[<Property>]
let ``Constructs BoolVector from bool value`` (x : bool) = (new BoolVector(x)).Length = 1

[<Property>]
let ``Constructs BoolVector from bool seq`` (len : int) = 
    len >= 0 ==> lazy ((new BoolVector(Array.create len false |> Array.toSeq)).Length = len)

//Throws index out of bounds??
//[<Property>]
//let ``Constructs BoolVector from int and initializer function`` (len : int) (x : int -> bool) = 
//    len >= 0 ==> lazy ((new BoolVector(len, x).Length = len)

[<Property>]
let ``Constructs BoolVector from bool array with copy`` (len : int) = 
    len >= 0 ==> lazy ((new BoolVector(Array.create len false, true)).Length = len)

[<Property>]
let ``Constructs BoolVector from bool array without copy`` (len : int) = 
    len >= 0 ==> lazy ((new BoolVector(Array.create len false, false)).Length = len)
