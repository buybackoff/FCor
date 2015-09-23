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

let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

let n = 2
let tmp = float(2)/float(2-1)
let factor = (tmp*tmp).ToString("N16")


let rng = new MT19937Rng()
let rnd = new Random()

let x = new BoolMatrix(2,3,true)
let y = new BoolMatrix(3,2,false)
let res = BoolMatrixExpr.EvalIn(x.AsExpr .&& y, None)
