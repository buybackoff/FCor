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

let inline (<=>) (x : Matrix) (y : Matrix) = epsEqualArray2D (x.ToArray2D()) (y.ToArray2D()) epsEqualFloat 0.0
let app = new MLAppClass()
let rnd = new Random()

let v = !![[7.;1.4]
           [3.;1.4]]:Matrix
setMatrix app "v" (v.ToArray2D())
app.Execute("res = kurtosis(v);") |> ignore
let res = getVector app "res"
let res2 = kurtosis v ColumnAxis

let piMC(n : int) =
    use rng1 = new MT19937Rng()
    use x1 = rand rng1 n : Vector
    use x2 = rand rng1 n : Vector
    use inCircle = (x1.AsExpr .^ 2) + (x2.AsExpr .^ 2) .<= 1.0 |> eval
    (inCircle.[inCircle].LongLength |> float)/float(n) * 4.0

MklControl.SetMaxThreads(4)
#time
let pi = piMC 100000000
Math.PI
    