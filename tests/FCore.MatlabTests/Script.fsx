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

//let binomAmerican (S0 : float) (K : float) (r : float) (vol : float) (T : float) n =
//    let dt = T / float(n)
//    let u = exp(vol * sqrt(dt))
//    let pUp = (u*exp(r*dt) - 1.0) / (u*u - 1.0) * exp(-r*dt)
//    let pDown = exp(-r*dt) - pUp
//    use u2 = new Vector(n+1, 1.0/(u*u))
//    u2.[0L] <- 1.0
//    use S = S0 *(u**float(n)) .* (cumprod u2)
//    use K2 = new Vector(n+1, K)
//    use V = K2 - S
//    evalIn (iif (V.AsExpr .< 0.0) !!0.0 V) V
//    let expr1 = S.AsExpr ./ u
//    let expr2 = VectorExpr.Max(V.AsExpr, (K2-S.AsExpr))
//    for i in n..(-1)..1 do
//        use V1 = V.View(0,i-1)
//        use V2 = V.View(1,i)
//        evalIn (pUp .* V1.AsExpr + pDown .* V2.AsExpr) V1
//        evalIn expr1 S
//        evalIn expr2 V
//    V.[0]
//
//#time
//MklControl.SetMaxThreads(1)
//for i in 1..100 do
//    let binOption = binomAmerican 44. 40. 0.06 0.4 1. 1000
//    ()
//
//let binOption = binomAmerican 44. 40. 0.06 0.4 1. 1000
 
//let n = 100
//let rng1 = new SOBOLRng(2)
//let rng2 = rng1.Copy()
//rng1.LeapFrog(0)
//rng2.LeapFrog(1)
//let x1 = rand rng1 n : Vector
//let x2 = rand rng2 n : Vector
//let res = (iif ((x1.AsExpr .^ 2) + (x2.AsExpr .^ 2) .<= 1.0) 1.0 0.0) |> eval



//let piMC(n : int) =
//    use rng1 = new SOBOLRng(2)
//    use rng2 = rng1.Copy()
//    rng1.LeapFrog(0)
//    rng2.LeapFrog(1)
//    use x1 = rand rng1 n : Vector
//    use x2 = rand rng2 n : Vector
//    use inCircle = (x1.AsExpr .^ 2) + (x2.AsExpr .^ 2) .<= 1.0 |> eval
//    use y = inCircle.[inCircle]
//    (y.LongLength |> float)/float(n) * 4.0
//
//#time
//MklControl.SetMaxThreads(4)
//let pi = piMC 100000000
//Math.PI

