namespace FCor.Tests

open FCor
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module BoolMatrixOperators = 
    let m1 = new BoolMatrix([[ true; true ];[ false; true ]])
    let m2 = new BoolMatrix([[ false; true ];[ false; false ]])
    
    [<Fact>]
    let ``Operator ==``() = m1 == m2 |> should equal false
    
    [<Fact>]
    let ``Operator !=``() = m1 != m2 |> should equal true
    
    [<Fact>]
    let ``Operator .&&``() = (m1 .&& m2).ToArray2D() |> should equal ([|[| false; true|] ; [|false;false |]|]|> array2D)
    
    [<Fact>]
    let ``Operator .||``() = (m1 .|| m2).ToArray2D() |> should equal ([|[| true; true|] ; [|false;true |]|]|> array2D)
