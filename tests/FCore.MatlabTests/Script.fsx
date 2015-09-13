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

let rng = new MT19937Rng()

let x = Array2D.create 0 0 false
let y = Array2D.create 1 0 false
let X = new BoolMatrix(x)
let Y = new BoolMatrix(y)
x = y
X = Y
