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

module RandomVector =

    let n = 10000000L

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[]) (y :float[])  = epsEqualArray x y epsEqualFloat eps

    [<Fact>]
    let ``Rand MCG31Rng`` () =
        use rng = new MCG31Rng()
        rng.LeapFrog(3, 10)
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead MCG31Rng`` () =
        use rng1 = new MCG31Rng()
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 

    [<Fact>]
    let ``Rand R250Rng`` () =
        use rng = new R250Rng()
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy R250Rng`` () =
        use rng1 = new R250Rng()
        use rng2 = rng1.Copy()
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 1000
        v1 = v2 

    [<Fact>]
    let ``Rand MRG32K3ARng`` () =
        use rng = new MRG32K3ARng()
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead MRG32K3ARng`` () =
        use rng1 = new MRG32K3ARng()
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 

    [<Fact>]
    let ``Rand MCG59Rng`` () =
        use rng = new MCG59Rng()
        rng.LeapFrog(3, 10)
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead MCG59Rng`` () =
        use rng1 = new MCG59Rng()
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 

    [<Fact>]
    let ``Rand WHRng`` () =
        use rng = new WHRng([||], 2)
        rng.LeapFrog(3, 10)
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead WHRng`` () =
        use rng1 = new WHRng([||], 2)
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 

    [<Fact>]
    let ``Rand MT19937Rng`` () =
        use rng = new MT19937Rng()
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead MT19937Rng`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 

    [<Fact>]
    let ``Rand SFMT19937Rng`` () =
        use rng = new SFMT19937Rng()
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead SFMT19937Rng`` () =
        use rng1 = new SFMT19937Rng()
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 

    [<Fact>]
    let ``Rand MT2203Rng`` () =
        use rng = new MT2203Rng([||], 2)
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy MT2203Rng`` () =
        use rng1 = new MT2203Rng([||], 2)
        use rng2 = rng1.Copy()
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 1000
        v1 = v2 

    [<Fact>]
    let ``Rand SOBOLRng`` () =
        use rng = new SOBOLRng(10)
        rng.LeapFrog(3)
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead SOBOLRng`` () =
        use rng1 = new SOBOLRng(2)
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 

    [<Fact>]
    let ``Rand NIEDERRRng`` () =
        use rng = new NIEDERRRng(10)
        rng.LeapFrog(3)
        use v : Vector = rand rng n
        let m = mean v
        let v = var v
        (epsEqualFloat m 0.5 1e-3) && (epsEqualFloat v (1./12.) 1e-3)

    [<Fact>]
    let ``Copy and SkipAhead NIEDERRRng`` () =
        use rng1 = new NIEDERRRng(2)
        use rng2 = rng1.Copy()
        rng2.SkipAhead(500L)
        use v1 : Vector = rand rng1 1000
        use v2 : Vector = rand rng2 500
        v1.[500..] = v2 


