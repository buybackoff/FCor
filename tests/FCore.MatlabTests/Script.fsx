#r "./bin/release/Interop.MLApp.dll"
#r "./bin/release/FCore.MatlabTests.dll"
#r "./bin/release/FCore.dll"

open MLApp
open System
open FCore
open FCore.BasicStats
open FCore.Math
open FCore.MatlabTests
open FCore.MatlabTests.Util

let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

let x = new Vector([|1000.0..(-1.0)..0.0|])
let q = new Vector([|1.0..(-0.1)..0.0|])
let m = quantile x q 
DisplayControl.MaxDisplaySize <- (20L,10L)