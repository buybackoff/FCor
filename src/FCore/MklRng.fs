namespace FCore
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

type MklBasicRng =
    ///<summary>31-bit multiplicative congruential generator
    ///</summary>
    | MCG31   = 0
    ///<summary>Generalized feedback shift register generator
    ///</summary>
    | R250    = 1
    ///<summary>Combined multiple recursive generator with two components of order 3
    ///</summary>
    | MRG32K3A  = 2
    ///<summary>59-bit multiplicative congruential generator
    ///</summary>
    | MCG59 = 3
    ///<summary>Set of 273 Wichmann-Hill combined multiplicative congruential generators
    ///</summary>
    | WH   = 4
    ///<summary>Mersenne Twister pseudorandom number generator
    ///</summary>
    | MT19937    = 5
    ///<summary>Set of 6024 Mersenne Twister pseudorandom number generators
    ///</summary>
    | MT2203  = 6
    ///<summary>SIMD-oriented Fast Mersenne Twister pseudorandom number generator
    ///</summary>
    | SFMT19937 = 7
    ///<summary>32-bit Gray code-based generator producing low-discrepancy sequences for dimensions 1 ≤ s ≤ 40
    ///</summary>
    | SOBOL  = 8
    ///<summary>32-bit Gray code-based generator producing low-discrepancy sequences for dimensions 1 ≤ s ≤ 318
    ///</summary>
    | NIEDERR = 9

type RandDummy = RandDummy
type UnifRndDummy = UnifRndDummy
type NormRndDummy = NormRndDummy
type ExponRndDummy = ExponRndDummy
type LaplaceRndDummy = LaplaceRndDummy
type WeibullRndDummy = WeibullRndDummy
type CauchyRndDummy = CauchyRndDummy
type RayleighRndDummy = RayleighRndDummy
type LognormRndDummy = LognormRndDummy
type GumbelRndDummy = GumbelRndDummy
type GammaRndDummy = GammaRndDummy
type BetaRndDummy = BetaRndDummy
type BernRndDummy = BernRndDummy
type GeomRndDummy = GeomRndDummy
type BinomRndDummy = BinomRndDummy
type HyperGeomRndDummy = HyperGeomRndDummy
type PoissonRndDummy = PoissonRndDummy
type NegbinomRndDummy = NegbinomRndDummy

type MklRng =

    val streamPtr : IntPtr

    new (streamPtr) = {streamPtr = streamPtr}

    static member op_Explicit(dummy : RandDummy, rng : MklRng) =
        fun (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, 0.0, 1.0, res.NativeArray)
            res

    static member op_Explicit(dummy : RandDummy, rng : MklRng) =
        fun (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, 0.0, 1.0, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : RandDummy, rng : MklRng) =
        fun (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, 0.0, 1.0, res.NativeArray)
            res

    static member op_Explicit(dummy : RandDummy, rng : MklRng) =
        fun (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, 0.0, 1.0, res.NativeArray)
            new Matrix(rows, cols, res)



    static member op_Explicit(dummy : UnifRndDummy, rng : MklRng) =
        fun a b (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, a, b, res.NativeArray)
            res

    static member op_Explicit(dummy : UnifRndDummy, rng : MklRng) =
        fun a b (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, a, b, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : UnifRndDummy, rng : MklRng) =
        fun a b (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, a, b, res.NativeArray)
            res

    static member op_Explicit(dummy : UnifRndDummy, rng : MklRng) =
        fun a b (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Uniform_Rnd(rng.streamPtr, size, a, b, res.NativeArray)
            new Matrix(rows, cols, res)



    static member op_Explicit(dummy : NormRndDummy, rng : MklRng) =
        fun mean sigma (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Norm_Rnd(rng.streamPtr, size, mean, sigma, res.NativeArray)
            res

    static member op_Explicit(dummy : NormRndDummy, rng : MklRng) =
        fun mean sigma (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Norm_Rnd(rng.streamPtr, size, mean, sigma, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : NormRndDummy, rng : MklRng) =
        fun mean sigma (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Norm_Rnd(rng.streamPtr, size, mean, sigma, res.NativeArray)
            res

    static member op_Explicit(dummy : NormRndDummy, rng : MklRng) =
        fun mean sigma (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Norm_Rnd(rng.streamPtr, size, mean, sigma, res.NativeArray)
            new Matrix(rows, cols, res)



    static member op_Explicit(dummy : ExponRndDummy, rng : MklRng) =
        fun alpha beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Expon_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : ExponRndDummy, rng : MklRng) =
        fun alpha beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Expon_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : ExponRndDummy, rng : MklRng) =
        fun alpha beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Expon_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : ExponRndDummy, rng : MklRng) =
        fun alpha beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Expon_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : LaplaceRndDummy, rng : MklRng) =
        fun alpha beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Laplace_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : LaplaceRndDummy, rng : MklRng) =
        fun alpha beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Laplace_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : LaplaceRndDummy, rng : MklRng) =
        fun alpha beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Laplace_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : LaplaceRndDummy, rng : MklRng) =
        fun alpha beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Laplace_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : WeibullRndDummy, rng : MklRng) =
        fun a alpha beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Weibull_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : WeibullRndDummy, rng : MklRng) =
        fun a alpha beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Weibull_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : WeibullRndDummy, rng : MklRng) =
        fun a alpha beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Weibull_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : WeibullRndDummy, rng : MklRng) =
        fun a alpha beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Weibull_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)



    static member op_Explicit(dummy : CauchyRndDummy, rng : MklRng) =
        fun alpha beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Cauchy_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : CauchyRndDummy, rng : MklRng) =
        fun alpha beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Cauchy_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : CauchyRndDummy, rng : MklRng) =
        fun alpha beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Cauchy_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : CauchyRndDummy, rng : MklRng) =
        fun alpha beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Cauchy_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)



    static member op_Explicit(dummy : RayleighRndDummy, rng : MklRng) =
        fun alpha beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Rayleigh_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : RayleighRndDummy, rng : MklRng) =
        fun alpha beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Rayleigh_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : RayleighRndDummy, rng : MklRng) =
        fun alpha beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Rayleigh_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : RayleighRndDummy, rng : MklRng) =
        fun alpha beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Rayleigh_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)



    static member op_Explicit(dummy : LognormRndDummy, rng : MklRng) =
        fun a sigma b beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Lognorm_Rnd(rng.streamPtr, size, a, sigma, b, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : LognormRndDummy, rng : MklRng) =
        fun a sigma b beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Lognorm_Rnd(rng.streamPtr, size, a, sigma, b, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : LognormRndDummy, rng : MklRng) =
        fun a sigma b beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Lognorm_Rnd(rng.streamPtr, size, a, sigma, b, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : LognormRndDummy, rng : MklRng) =
        fun a sigma b beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Lognorm_Rnd(rng.streamPtr, size, a, sigma, b, beta, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : GumbelRndDummy, rng : MklRng) =
        fun alpha beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gumbel_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : GumbelRndDummy, rng : MklRng) =
        fun alpha beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gumbel_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : GumbelRndDummy, rng : MklRng) =
        fun alpha beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gumbel_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : GumbelRndDummy, rng : MklRng) =
        fun alpha beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gumbel_Rnd(rng.streamPtr, size, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : GammaRndDummy, rng : MklRng) =
        fun a alpha beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gamma_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : GammaRndDummy, rng : MklRng) =
        fun a alpha beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gamma_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : GammaRndDummy, rng : MklRng) =
        fun a alpha beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gamma_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : GammaRndDummy, rng : MklRng) =
        fun a alpha beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Gamma_Rnd(rng.streamPtr, size, a, alpha, beta, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : BetaRndDummy, rng : MklRng) =
        fun p q a beta (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Beta_Rnd(rng.streamPtr, size, p, q, a, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : BetaRndDummy, rng : MklRng) =
        fun p q a beta (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Beta_Rnd(rng.streamPtr, size, p, q, a, beta, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : BetaRndDummy, rng : MklRng) =
        fun p q a beta (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Beta_Rnd(rng.streamPtr, size, p, q, a, beta, res.NativeArray)
            res

    static member op_Explicit(dummy : BetaRndDummy, rng : MklRng) =
        fun p q a beta (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Beta_Rnd(rng.streamPtr, size, p, q, a, beta, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : BernRndDummy, rng : MklRng) =
        fun p (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Bern_Rnd(rng.streamPtr, size, p, res.NativeArray)
            res

    static member op_Explicit(dummy : BernRndDummy, rng : MklRng) =
        fun p (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Bern_Rnd(rng.streamPtr, size, p, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : BernRndDummy, rng : MklRng) =
        fun p (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Bern_Rnd(rng.streamPtr, size, p, res.NativeArray)
            res

    static member op_Explicit(dummy : BernRndDummy, rng : MklRng) =
        fun p (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Bern_Rnd(rng.streamPtr, size, p, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : GeomRndDummy, rng : MklRng) =
        fun p (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Geom_Rnd(rng.streamPtr, size, p, res.NativeArray)
            res

    static member op_Explicit(dummy : GeomRndDummy, rng : MklRng) =
        fun p (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Geom_Rnd(rng.streamPtr, size, p, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : GeomRndDummy, rng : MklRng) =
        fun p (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Geom_Rnd(rng.streamPtr, size, p, res.NativeArray)
            res

    static member op_Explicit(dummy : GeomRndDummy, rng : MklRng) =
        fun p (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Geom_Rnd(rng.streamPtr, size, p, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : BinomRndDummy, rng : MklRng) =
        fun ntrial p (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Binom_Rnd(rng.streamPtr, size, ntrial, p, res.NativeArray)
            res

    static member op_Explicit(dummy : BinomRndDummy, rng : MklRng) =
        fun ntrial p (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Binom_Rnd(rng.streamPtr, size, ntrial, p, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : BinomRndDummy, rng : MklRng) =
        fun ntrial p (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Binom_Rnd(rng.streamPtr, size, ntrial, p, res.NativeArray)
            res

    static member op_Explicit(dummy : BinomRndDummy, rng : MklRng) =
        fun ntrial p (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Binom_Rnd(rng.streamPtr, size, ntrial, p, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : HyperGeomRndDummy, rng : MklRng) =
        fun l s m (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Hypergeom_Rnd(rng.streamPtr, size, l, s, m, res.NativeArray)
            res

    static member op_Explicit(dummy : HyperGeomRndDummy, rng : MklRng) =
        fun l s m (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Hypergeom_Rnd(rng.streamPtr, size, l, s, m, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : HyperGeomRndDummy, rng : MklRng) =
        fun l s m (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Hypergeom_Rnd(rng.streamPtr, size, l, s, m, res.NativeArray)
            res

    static member op_Explicit(dummy : HyperGeomRndDummy, rng : MklRng) =
        fun l s m (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Hypergeom_Rnd(rng.streamPtr, size, l, s, m, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : PoissonRndDummy, rng : MklRng) =
        fun lambda (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Poisson_Rnd(rng.streamPtr, size, lambda, res.NativeArray)
            res

    static member op_Explicit(dummy : PoissonRndDummy, rng : MklRng) =
        fun lambda (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Poisson_Rnd(rng.streamPtr, size, lambda, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : PoissonRndDummy, rng : MklRng) =
        fun lambda (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Poisson_Rnd(rng.streamPtr, size, lambda, res.NativeArray)
            res

    static member op_Explicit(dummy : PoissonRndDummy, rng : MklRng) =
        fun lambda (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Poisson_Rnd(rng.streamPtr, size, lambda, res.NativeArray)
            new Matrix(rows, cols, res)


    static member op_Explicit(dummy : NegbinomRndDummy, rng : MklRng) =
        fun a p (size : int64) ->
            let res = new Vector(size, 0.0)
            MklFunctions.D_Negbinom_Rnd(rng.streamPtr, size, a, p, res.NativeArray)
            res

    static member op_Explicit(dummy : NegbinomRndDummy, rng : MklRng) =
        fun a p (rows : int64) (cols : int64) ->
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Negbinom_Rnd(rng.streamPtr, size, a, p, res.NativeArray)
            new Matrix(rows, cols, res)

    static member op_Explicit(dummy : NegbinomRndDummy, rng : MklRng) =
        fun a p (size : int) ->
            let size = size |> int64
            let res = new Vector(size, 0.0)
            MklFunctions.D_Negbinom_Rnd(rng.streamPtr, size, a, p, res.NativeArray)
            res

    static member op_Explicit(dummy : NegbinomRndDummy, rng : MklRng) =
        fun a p (rows : int) (cols : int) ->
            let rows, cols = rows |> int64, cols |> int64
            let size = rows * cols
            let res = new Vector(size, 0.0)
            MklFunctions.D_Negbinom_Rnd(rng.streamPtr, size, a, p, res.NativeArray)
            new Matrix(rows, cols, res)

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = 
        if isDisposing then GC.SuppressFinalize(this)
        MklFunctions.Delete_Rng(this.streamPtr)

    override this.Finalize() = try this.DoDispose(false) with _ -> ()

type MCG31Rng =
    inherit MklRng

    new (seeds : uint32[]) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.MCG31), seeds, 0, 0))}

    new (seed : uint32) = new MCG31Rng([|seed|])

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new MCG31Rng([||])
    
    member this.Copy() =
        new MCG31Rng(MklFunctions.Copy_Rng(this.streamPtr))   

    member this.LeapFrog(k, nStreams) =
        MklFunctions.Leapfrog_Rng(this.streamPtr, k, nStreams)

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)


type R250Rng =
    inherit MklRng

    new (seeds : uint32[]) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.R250), seeds, 0, 0))}

    new (seed : uint32) = new R250Rng([|seed|])

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new R250Rng([||])  
    
    member this.Copy() =
        new R250Rng(MklFunctions.Copy_Rng(this.streamPtr)) 


type MRG32K3ARng =
    inherit MklRng

    new (seeds : uint32[]) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.MRG32K3A), seeds, 0, 0))}

    new (seed : uint32) = new MRG32K3ARng([|seed|])

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new MRG32K3ARng([||])  
    
    member this.Copy() =
        new MRG32K3ARng(MklFunctions.Copy_Rng(this.streamPtr)) 

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)


type MCG59Rng =
    inherit MklRng

    new (seeds : uint32[]) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.MCG59), seeds, 0, 0))}

    new (seed : uint32) = new MCG59Rng([|seed|])

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new MCG59Rng([||])  
    
    member this.Copy() =
        new MCG59Rng(MklFunctions.Copy_Rng(this.streamPtr)) 

    member this.LeapFrog(k, nStreams) =
        MklFunctions.Leapfrog_Rng(this.streamPtr, k, nStreams)

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)


type WHRng =
    inherit MklRng

    new (seeds : uint32[], index) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.WH), seeds, index, 0))}

    new (seed : uint32, index) = new WHRng([|seed|], index)

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new (index) = new WHRng([||], index)  

    new (seeds : uint32[]) = new WHRng(seeds, 0)

    new (seed : uint32) = new WHRng(seed, 0)

    new () = new WHRng(0)
    
    member this.Copy() =
        new WHRng(MklFunctions.Copy_Rng(this.streamPtr)) 

    member this.LeapFrog(k, nStreams) =
        MklFunctions.Leapfrog_Rng(this.streamPtr, k, nStreams)

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)


type MT19937Rng =
    inherit MklRng

    new (seeds : uint32[]) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.MT19937), seeds, 0, 0))}

    new (seed : uint32) = new MT19937Rng([|seed|])

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new MT19937Rng([||])
    
    member this.Copy() =
        new MT19937Rng(MklFunctions.Copy_Rng(this.streamPtr)) 

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)


type SFMT19937Rng =
    inherit MklRng

    new (seeds : uint32[]) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.SFMT19937), seeds, 0, 0))}

    new (seed : uint32) = new SFMT19937Rng([|seed|])

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new SFMT19937Rng([||])
    
    member this.Copy() =
        new SFMT19937Rng(MklFunctions.Copy_Rng(this.streamPtr)) 

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)


type MT2203Rng =
    inherit MklRng

    new (seeds : uint32[], index) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.MT2203), seeds, index, 0))}

    new (seed : uint32, index) = new MT2203Rng([|seed|], index)

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new (index) = new MT2203Rng([||], index)  

    new (seeds : uint32[]) = new MT2203Rng(seeds, 0)

    new (seed : uint32) = new MT2203Rng(seed, 0)

    new () = new MT2203Rng(0)
    
    member this.Copy() =
        new MT2203Rng(MklFunctions.Copy_Rng(this.streamPtr)) 


type SOBOLRng =
    inherit MklRng

    new (dimension) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.SOBOL), [||], 0, dimension))}

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new SOBOLRng(1)
    
    member this.Copy() =
        new SOBOLRng(MklFunctions.Copy_Rng(this.streamPtr)) 

    member this.LeapFrog(k) =
        let nStreams = 0x7fffffff
        MklFunctions.Leapfrog_Rng(this.streamPtr, k, nStreams)

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)

type NIEDERRRng =
    inherit MklRng

    new (dimension) = {inherit MklRng(MklFunctions.Create_Rng(int(MklBasicRng.NIEDERR), [||], 0, dimension))}

    new (streamPtr : IntPtr) = {inherit MklRng(streamPtr)}

    new () = new NIEDERRRng(1)
    
    member this.Copy() =
        new NIEDERRRng(MklFunctions.Copy_Rng(this.streamPtr)) 

    member this.LeapFrog(k) =
        let nStreams = 0x7fffffff
        MklFunctions.Leapfrog_Rng(this.streamPtr, k, nStreams)

    member this.SkipAhead(nSkip : int64) =
        MklFunctions.Skipahead_Rng(this.streamPtr, nSkip)

    member this.SkipAhead(nSkip : int) =
        this.SkipAhead(nSkip |> int64)



