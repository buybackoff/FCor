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

module RandomMatrix =

    let n = 10000000L
    let rows = 1000L
    let cols = 10000L

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[]) (y :float[])  = epsEqualArray x y epsEqualFloat eps

    [<Fact>]
    let ``Rand`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let randV : int64 -> Vector = rand rng1
        let randM : int64 -> int64 -> Matrix = rand rng2
        use v = randV n
        use m = randM rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``Unifrnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let unifRndV : float -> float -> int64 -> Vector = unifRnd rng1
        let unifRndM : float -> float -> int64 -> int64 -> Matrix = unifRnd rng2
        use v = unifRndV 1.234 2.345 n
        use m = unifRndM 1.234 2.345 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``Normrnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let normRndV : float -> float -> int64 -> Vector = normRnd rng1
        let normRndM : float -> float -> int64 -> int64 -> Matrix = normRnd rng2
        use v = normRndV 1.1234 2.5432 n
        use m = normRndM 1.1234 2.5432 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``Exponrnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let exponRndV : float -> float -> int64 -> Vector = exponRnd rng1
        let exponRndM : float -> float -> int64 -> int64 -> Matrix = exponRnd rng2
        use v = exponRndV 1.1234 2.5432 n
        use m = exponRndM 1.1234 2.5432 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``LaplaceRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let laplaceRndV : float -> float -> int64 -> Vector = laplaceRnd rng1
        let laplaceRndM : float -> float -> int64 -> int64 -> Matrix = laplaceRnd rng2
        use v = laplaceRndV 1.1234 2.5432 n
        use m = laplaceRndM 1.1234 2.5432 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``WeibullRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let weibullRndV : float -> float -> float -> int64 -> Vector = weibullRnd rng1
        let weibullRndM : float -> float -> float -> int64 -> int64 -> Matrix = weibullRnd rng2
        use v = weibullRndV 1.1234 0.5 2.5432 n
        use m = weibullRndM 1.1234 0.5 2.5432 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``CauchyRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let cauchyRndV : float -> float -> int64 -> Vector = cauchyRnd rng1
        let cauchyRndM : float -> float -> int64 -> int64 -> Matrix = cauchyRnd rng2
        use v = cauchyRndV 1.1234 2.5432 n
        use m = cauchyRndM 1.1234 2.5432 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``RayleighRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let rayleighRndV : float -> float -> int64 -> Vector = rayleighRnd rng1
        let rayleighRndM : float -> float -> int64 -> int64 -> Matrix = rayleighRnd rng2
        use v = rayleighRndV 1.1234 2.5432 n
        use m = rayleighRndM 1.1234 2.5432 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``LognormRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let lognormRndV : float -> float -> float -> float -> int64 -> Vector = lognormRnd rng1
        let lognormRndM : float -> float -> float -> float -> int64 -> int64 -> Matrix = lognormRnd rng2
        use v = lognormRndV 0.1234 0.5432 0.5666 0.876 n
        use m = lognormRndM 0.1234 0.5432 0.5666 0.876 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``GumbelRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let gumbelRndV : float -> float -> int64 -> Vector = gumbelRnd rng1
        let gumbelRndM : float -> float -> int64 -> int64 -> Matrix = gumbelRnd rng2
        use v = gumbelRndV 1.1234 2.5432 n
        use m = gumbelRndM 1.1234 2.5432 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``GammaRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let gammaRndV : float -> float -> float -> int64 -> Vector = gammaRnd rng1
        let gammaRndM : float -> float -> float -> int64 -> int64 -> Matrix = gammaRnd rng2
        use v = gammaRndV 1.1234 2.5432 3.321 n
        use m = gammaRndM 1.1234 2.5432 3.321 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``betaRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let betaRndV : float -> float -> float -> float -> int64 -> Vector = betaRnd rng1
        let betaRndM : float -> float -> float -> float -> int64 -> int64 -> Matrix = betaRnd rng2
        use v = betaRndV 1.1234 2.5432 3.321 4.123 n
        use m = betaRndM 1.1234 2.5432 3.321 4.123 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``BernRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let bernRndV : float -> int64 -> Vector = bernRnd rng1
        let bernRndM : float -> int64 -> int64 -> Matrix = bernRnd rng2
        use v = bernRndV 0.232 n
        use m = bernRndM 0.232 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``GeomRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let geomRndV : float -> int64 -> Vector = geomRnd rng1
        let geomRndM : float -> int64 -> int64 -> Matrix = geomRnd rng2
        use v = geomRndV 0.232 n
        use m = geomRndM 0.232 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``BinomRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let binomRndV : int -> float -> int64 -> Vector = binomRnd rng1
        let binomRndM : int -> float -> int64 -> int64 -> Matrix = binomRnd rng2
        use v = binomRndV 123 0.232 n
        use m = binomRndM 123 0.232 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``HypergeomRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let hyperGeomRndV : int -> int -> int -> int64 -> Vector = hyperGeomRnd rng1
        let hyperGeomRndM : int -> int -> int -> int64 -> int64 -> Matrix = hyperGeomRnd rng2
        use v = hyperGeomRndV 123 52 11 n
        use m = hyperGeomRndM 123 52 11 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``PoissRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let poissonRndV : float -> int64 -> Vector = poissonRnd rng1
        let poissonRndM : float -> int64 -> int64 -> Matrix = poissonRnd rng2
        use v = poissonRndV 2.243 n
        use m = poissonRndM 2.243 rows cols
        v = m.ColMajorDataVector

    [<Fact>]
    let ``NegbinRnd`` () =
        use rng1 = new MT19937Rng()
        use rng2 = rng1.Copy()
        let negbinomRndV : float ->float -> int64 -> Vector = negbinomRnd rng1
        let negbinomRndM : float ->float -> int64 -> int64 -> Matrix = negbinomRnd rng2
        use v = negbinomRndV 15.56 0.232 n
        use m =  negbinomRndM 15.56 0.232 rows cols
        v = m.ColMajorDataVector

