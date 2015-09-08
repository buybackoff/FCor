#r "./bin/release/Interop.MLApp.dll"
#r "./bin/release/FCore.MatlabTests.dll"
#r "./bin/release/FCore.dll"

open MLApp
open System
open System.Runtime.InteropServices
open System.Reflection
open FCore
open FCore.BasicStats
open FCore.Math
open FCore.MatlabTests
open FCore.MatlabTests.Util

let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0
let x = new Vector([|1./3.;1./3.|])
let s = skewness x