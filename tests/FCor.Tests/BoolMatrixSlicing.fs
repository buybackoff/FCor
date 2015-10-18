namespace FCor.Tests

open FCor
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolMatrixSlicing = 
    let m1 = new BoolMatrix([[ true; true ];[ false; true ]])
    
    [<Fact>]
    let ``Slice from 1``() = m1.[1..1,0..1].ToArray2D().[0,*] |> should equal [| false; true |]
