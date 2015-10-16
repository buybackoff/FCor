#r "./bin/release/Interop.MLApp.dll"
#r "./bin/release/FCore.MatlabTests.dll"
#r "./bin/release/FCore.dll"

open MLApp
open System
open System.Runtime.InteropServices
open System.Reflection
open FCore
open FCore.BasicStats
open FCore.Random
open FCore.Math
open FCore.LinearAlgebra
open FCore.MatlabTests
open FCore.MatlabTests.Util
open FCore.ExplicitConversion

let lsmPutAmerican (S0 : float) (K : float) (r : float) (vol : float) (T : float) (n : int) (sims : int) =
    let dt = T / float(n)
    let N = n + 1
    let mu = (r-vol*vol/2.0)*dt
    let sigma = vol*sqrt(dt)
    use rng = new MT19937Rng()
    let lognormRndM : float -> float -> float -> float -> int -> int -> Matrix = lognormRnd rng
    use R = lognormRndM mu sigma 0.0 1.0 sims N
    R.[*, 0] <- S0 
    use S = cumprod R RowAxis
    use CF = VectorExpr.Max(K - S.ColView(N-1).AsExpr, 0.0) |> eval
    let discF = exp(-r*dt)
    for i in N-2..-1..1 do
        use Si = S.ColView(i)
        use inMoney = Si .< K
        use outMoney = BoolVector.Not inMoney 
        use X = Si.[inMoney]
        use Y = CF.[inMoney] * discF
        if X.LongLength > 0L then
            use XReg = new Matrix(X.LongLength, 3L, 0.0)
            XReg.[*, 0] <- 1.0
            XReg.[*, 1] <- X
            XReg.[*, 2] <- X .^ 2
            use a = qrSolveFull XReg Y
            use C = XReg * a
            use payoff = K - X
            use isExercised = payoff .> C
            Y.[isExercised] <- payoff.[isExercised]
            CF.[inMoney] <- Y
        CF.[outMoney] <- CF.[outMoney] * discF
    mean (CF * discF)


#time
MklControl.SetMaxThreads(4)
for i in 1..10 do
    let lsmOption = lsmPutAmerican 44.0 40.0 0.06 0.4 1.0 50 100000
    ()

let res = lsmPutAmerican 44.0 40.0 0.06 0.4 1.0 50 100000

let binomAmerican (S0 : float) (K : float) (r : float) (vol : float) (T : float) n =
    let dt = T / float(n)
    let u = exp(vol * sqrt(dt))
    let pUp = (u*exp(r*dt) - 1.0) / (u*u - 1.0) * exp(-r*dt)
    let pDown = exp(-r*dt) - pUp
    use u2 = new Vector(n+1, 1.0/(u*u))
    u2.[0L] <- 1.0
    use S = S0 *(u**float(n)) * (cumprod u2)
    use K2 = new Vector(n+1, K)
    use V = K2 - S
    evalIn (iif (V.AsExpr .< 0.0) !!0.0 V) V
    let expr1 = S.AsExpr / u
    let expr2 = VectorExpr.Max(V.AsExpr, (K2-S.AsExpr))
    for i in n..(-1)..1 do
        use V1 = V.View(0,i-1)
        use V2 = V.View(1,i)
        evalIn (pUp * V1.AsExpr + pDown * V2.AsExpr) V1
        evalIn expr1 S
        evalIn expr2 V
    V.[0]

MklControl.SetMaxThreads(4)
for i in 1..100 do
    let binOption = binomAmerican 44. 40. 0.06 0.4 1. 1000
    ()

let binOption = binomAmerican 44. 40. 0.06 0.4 1. 1000
 




//let piMC(n : int) =
////    use rng1 = new SOBOLRng(2)
////    use rng2 = rng1.Copy()
////    rng1.LeapFrog(0)
////    rng2.LeapFrog(1)
//    use rng = new MT19937Rng()
//    use x1 = rand rng n : Vector
//    use x2 = rand rng n : Vector
//    use inCircle = (x1.AsExpr .^ 2) + (x2.AsExpr .^ 2) .<= 1.0 |> eval
//    // y = inCircle.[inCircle]
//    (inCircle.[inCircle].LongLength |> float)/float(n) * 4.0
//
//#time
//MklControl.SetMaxThreads(4)
//let pi = piMC 100000000
//Math.PI

