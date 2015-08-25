module FCore.Tests

open FCore
open Util
open Xunit
open FsCheck
open FsCheck.Xunit
open System

[<Property>]
let ``Constructs BoolVector from int64 and bool value`` (len : int64) x =
    len >= 0L ==> lazy((new BoolVector(len, x)).LongLength = len)

