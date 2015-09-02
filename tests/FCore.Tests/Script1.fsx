#r "./bin/release/Interop.MLApp.dll"
#r "./bin/release/FCore.Tests.dll"
#r "./bin/release/FCore.dll"

open MLApp
open System
open FCore
open FCore.Tests
open FCore.Tests.Util

let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

let x = [|1.0;Double.NaN|]
let v1 = new Vector(x)
let v2 = new Vector([|1.0;12.|])
let res1 = v1 .^ v2
Math.Pow(Double.NaN, 12.)



