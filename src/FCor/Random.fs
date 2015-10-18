namespace FCor

open System
open Overloading

module Random =

    let inline rand rng = explicitOverload< 'T, 'S, RandDummy> rng

    let inline unifRnd rng = explicitOverload< 'T, 'S, UnifRndDummy> rng

    let inline normRnd rng = explicitOverload< 'T, 'S, NormRndDummy> rng

    let inline exponRnd rng = explicitOverload< 'T, 'S, ExponRndDummy> rng

    let inline laplaceRnd rng = explicitOverload< 'T, 'S, LaplaceRndDummy> rng

    let inline weibullRnd rng = explicitOverload< 'T, 'S, WeibullRndDummy> rng

    let inline cauchyRnd rng = explicitOverload< 'T, 'S, CauchyRndDummy> rng

    let inline rayleighRnd rng = explicitOverload< 'T, 'S, RayleighRndDummy> rng

    let inline lognormRnd rng = explicitOverload< 'T, 'S, LognormRndDummy> rng

    let inline gumbelRnd rng = explicitOverload< 'T, 'S, GumbelRndDummy> rng

    let inline gammaRnd rng = explicitOverload< 'T, 'S, GammaRndDummy> rng

    let inline betaRnd rng = explicitOverload< 'T, 'S, BetaRndDummy> rng

    let inline bernRnd rng = explicitOverload< 'T, 'S, BernRndDummy> rng

    let inline geomRnd rng = explicitOverload< 'T, 'S, GeomRndDummy> rng

    let inline binomRnd rng = explicitOverload< 'T, 'S, BinomRndDummy> rng

    let inline hyperGeomRnd rng = explicitOverload< 'T, 'S, HyperGeomRndDummy> rng

    let inline poissonRnd rng = explicitOverload< 'T, 'S, PoissonRndDummy> rng

    let inline negbinomRnd rng = explicitOverload< 'T, 'S, NegbinomRndDummy> rng



