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

let inline (<=>) (x : Vector) (y : Vector) = epsEqualArray (x.ToArray()) (y.ToArray()) epsEqualFloat 0.0
//let app = new MLAppClass()

#time
let rng = new MT19937Rng()
let v1 = (rand rng 100000000 : Vector).AsExpr
let v2 = (rand rng 100000000 : Vector).AsExpr
Vector.EvalSliceLength <- 80000
MklControl.SetMaxThreads(8)
let res = (2.0 .* v1 + 3.0 .* v2) |> eval
