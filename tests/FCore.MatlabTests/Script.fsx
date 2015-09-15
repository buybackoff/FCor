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
let app = new MLAppClass()
let v = Array2D.init 1 1 (fun i j -> rnd.NextDouble() < 0.5)
setBoolMatrix app "v" v
let fromIndex = rnd.Next(v.Length) |> int64
let toIndex = rnd.Next(v.Length) |> int64
setScalar app "fromIndex" (float(fromIndex + 1L))
setScalar app "toIndex" (float(toIndex + 1L))
app.Execute("res = v(fromIndex:toIndex);") |> ignore
let res = getBoolVector app "res"
let V = new BoolMatrix(v)
V.[fromIndex..toIndex].ToArray() = res 


