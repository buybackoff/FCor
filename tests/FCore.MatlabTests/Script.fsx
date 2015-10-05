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
let rnd = new Random()

let v1 = rand rng 100000000 : Vector
let v2 = rand rng 100000000 : Vector
//let res = new Vector(100000000, 0.0)
MklControl.SetMaxThreads(4)
let e = eval (2.2.*v1.AsExpr + 3.3.* v2.AsExpr)
    
