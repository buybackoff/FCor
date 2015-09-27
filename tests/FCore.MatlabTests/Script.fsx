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

let n = 2
let tmp = float(2)/float(2-1)
let factor = (tmp*tmp).ToString("N16")


let rng = new MT19937Rng()
let rnd = new Random()
let a1 = Double.MaxValue
let a2 = 12.
let b1 = Double.PositiveInfinity
let b2 = -4.
let x1 = new Vector(a1)
let x2 = new Vector(a2)
let y1 = new Vector(b1)
let y2 = new Vector(b2)
let res1 = eval ((x1.AsExpr .* x2) + (y1.AsExpr .* y2))
let res2 = new Vector( (a1 * a2) + (b1 * b2))
res1 <=> res2
