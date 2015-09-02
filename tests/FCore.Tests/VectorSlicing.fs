namespace FCore.Tests

open FCore
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module VectorSlicing = 
    let v1 = new Vector([ 0.1; 0.2; 0.3 ])
    
    [<Fact>]
    let ``Slice from 1``() = v1.[1..2].ToArray() |> should equal [| 0.2; 0.3 |]
