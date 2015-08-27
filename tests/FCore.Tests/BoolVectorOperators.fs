namespace FCore.Tests

open FCore
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolVectorOperators = 
    let v1 = new BoolVector([ false; true; false ])
    let v2 = new BoolVector([ false; true; true ])
    
    [<Fact>]
    let ``Operator ==``() = v1 == v2 |> should equal false
    
    [<Fact>]
    let ``Operator !=``() = v1 != v2 |> should equal true
    
    [<Fact>]
    let ``Operator .&&``() = (v1 .&& v2).ToArray() |> should equal [| false; true; false |]
    
    [<Fact>]
    let ``Operator .||``() = (v1 .|| v2).ToArray() |> should equal [| false; true; true |]
