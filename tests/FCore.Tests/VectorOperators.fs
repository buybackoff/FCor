namespace FCore.Tests

open FCore
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System

module VectorOperators = 
    //need to handle rounding
    let cln (x : float) = x.ToString("F4")
    let v1 = new Vector([ 0.1; 0.2; 0.3 ])
    let v2 = new Vector([ 0.1; 0.2; 0.4 ])
    
    [<Fact>]
    let ``Operator +``() = 
        (v1 + v2).ToArray()
        |> Array.map cln
        |> should equal ([| 0.2; 0.4; 0.7 |] |> Array.map cln)
    
    [<Fact>]
    let ``Operator -``() = 
        (v1 - v2).ToArray()
        |> Array.map cln
        |> should equal ([| 0.0; 0.0; -0.1 |] |> Array.map cln)
    
    [<Fact>]
    let ``Operator .*``() = 
        (v1 .* v2).ToArray()
        |> Array.map cln
        |> should equal ([| 0.01; 0.04; 0.12 |] |> Array.map cln)
    
    [<Fact>]
    let ``Operator ./``() = 
        (v1 ./ v2).ToArray()
        |> Array.map cln
        |> should equal ([| 1.0; 1.0; 0.75 |] |> Array.map cln)
    
    [<Fact>]
    let ``Operator .^``() = 
        (v1 .^ v2).ToArray()
        |> Array.map cln
        |> should equal ([| 0.7943; 0.7248; 0.6178 |] |> Array.map cln)
    
    [<Fact>]
    let ``Operator unary -``() = 
        (-v2).ToArray()
        |> Array.map cln
        |> should equal ([| -0.1; -0.2; -0.4 |] |> Array.map cln)
    
//    [<Fact>]
//    let ``Operator ==``() = (v1 == v2) |> should equal false
//    
//    [<Fact>]
//    let ``Operator !=``() = (v1 != v2) |> should equal true
    
    [<Fact>]
    let ``Operator .<``() = (v1 .< v2).ToArray() |> should equal [| false; false; true |]
    
    [<Fact>]
    let ``Operator .<=``() = (v1 .<= v2).ToArray() |> should equal [| true; true; true |]
    
    [<Fact>]
    let ``Operator .>``() = (v1 .> v2).ToArray() |> should equal [| false; false; false |]
    
    [<Fact>]
    let ``Operator .>=``() = (v1 .>= v2).ToArray() |> should equal [| true; true; false |]
    
    [<Fact>]
    let ``Operator .<>``() = (v1 .<> v2).ToArray() |> should equal [| false; false; true |]
