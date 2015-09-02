namespace FCore.Tests

open FCore
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolVectorSlicing = 
    let v1 = new BoolVector([ false; true; false ])
    
    [<Fact>]
    let ``Slice from 1``() = v1.[1..2].ToArray() |> should equal [| true; false |]
