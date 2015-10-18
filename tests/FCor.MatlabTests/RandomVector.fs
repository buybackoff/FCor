namespace FCor.MatlabTests

open FCor
open FCor.Math
open FCor.BasicStats
open FCor.Random
open FCor.ExplicitConversion
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

    [<Fact>]
    let ``Unifrnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = unifRnd rng 1.234 2.345 n
        let m = mean v
        let v = var v
        let trueMean = (1.234 + 2.345)/2.
        let trueVar = 1./12.*(2.345 - 1.234)*(2.345 - 1.234)
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``Normrnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = normRnd rng 1.1234 2.5432 n
        let m = mean v
        let v = var v
        let trueMean = 1.1234
        let trueVar = 2.5432*2.5432
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``Exponrnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = exponRnd rng 1.1234 2.5432 n
        let m = mean v
        let v = var v
        let trueMean = 1.1234 + 2.5432
        let trueVar = 2.5432*2.5432
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``LaplaceRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = laplaceRnd rng 1.1234 2.5432 n
        let m = mean v
        let v = var v
        let trueMean = 1.1234
        let trueVar = 2.*2.5432*2.5432
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``WeibullRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = weibullRnd rng 1.1234 0.5 2.5432 n
        let m = mean v
        let v = var v
        let trueMean = 1.1234 + 2.5432* 2.
        let trueVar = 20. * 2.5432 * 2.5432
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``CauchyRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = cauchyRnd rng 1.1234 2.5432 n
        let m = quantile v !!0.5
        let trueMedian = 1.1234 
        (epsEqualFloat !!m trueMedian 1e-3) 

    [<Fact>]
    let ``RayleighRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = rayleighRnd rng 1.1234 2.5432 n
        let m = mean v
        let v = var v
        let trueMean = 1.1234 + 2.5432 * Math.Sqrt(Math.PI)/2.
        let trueVar = (4. - Math.PI)/4. * 2.5432 * 2.5432
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``LognormRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = lognormRnd rng 0.1234 0.5432 0.5666 0.876 n
        let m = mean v
        let v = var v
        let trueMean = 0.876*(Math.Exp(0.1234 + 0.5432 * 0.5432 / 2.)) + 0.5666
        let trueVar = (Math.Exp(0.5432* 0.5432) - 1.)* Math.Exp(2.*0.1234 + 0.5432*0.5432)* 0.876* 0.876
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``GumbelRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = gumbelRnd rng 1.1234 2.5432 n
        let m = mean v
        let v = var v
        let trueMean = 1.1234 -  2.5432 * 0.57721566490153286060
        let trueVar = Math.PI * Math.PI / 6. * 2.5432 * 2.5432
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``GammaRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = gammaRnd rng 1.1234 2.5432 3.321 n
        let m = mean v
        let v = var v
        let trueMean = 3.321 *  2.5432 +  1.1234
        let trueVar = 2.5432 * 3.321 * 3.321
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``betaRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = betaRnd rng 1.1234 2.5432 3.321 4.123 n
        let m = mean v
        let v = var v
        let trueMean = 4.123*1.1234 / (1.1234 + 2.5432) + 3.321
        let trueVar = 4.123 * 4.123 * 1.1234 * 2.5432/ ((1.1234 + 2.5432)*(1.1234 + 2.5432)*(1.1234 + 2.5432 + 1.))
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)


    [<Fact>]
    let ``BernRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = bernRnd rng 0.232 n
        let m = mean v
        let v = var v
        let trueMean = 0.232
        let trueVar = 0.232 * (1. - 0.232)
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``GeomRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = geomRnd rng 0.232 n
        let m = mean v
        let v = var v
        let trueMean = (1. - 0.232)/0.232
        let trueVar = (1. - 0.232)/(0.232*0.232)
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``BinomRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = binomRnd rng 123 0.232 n
        let m = mean v
        let v = var v
        let trueMean = 123.*0.232
        let trueVar = 123.*0.232*(1. - 0.232)
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``HypergeomRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = hyperGeomRnd rng 123 52 11 n
        let m = mean v
        let v = var v
        let trueMean = 52.* 11. /123.
        let trueVar = 52. * 11. * (123. - 52.)*(123. - 11.)/(123.*123.*(123.-1.))
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``PoissRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = poissonRnd rng 2.243 n
        let m = mean v
        let v = var v
        let trueMean = 2.243
        let trueVar = 2.243
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)

    [<Fact>]
    let ``NegbinRnd`` () =
        use rng = new MT19937Rng()
        use v : Vector = negbinomRnd rng 15.56 0.232 n
        let m = mean v
        let v = var v
        let trueMean = 15.56 * (1. - 0.232) / 0.232
        let trueVar = 15.56 * (1. - 0.232) / (0.232*0.232)
        (epsEqualFloat m trueMean 1e-3) && (epsEqualFloat v trueVar 1e-3)


























