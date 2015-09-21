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

let axisNum (axis :  MatrixAxis) =
    match axis with | ColumnAxis -> 1.0 | RowAxis -> 2.0

let v = new Matrix([[Double.PositiveInfinity;Double.PositiveInfinity;Double.PositiveInfinity;Double.PositiveInfinity;Double.PositiveInfinity]
                    [Double.NaN;Double.NaN;Double.NaN;Double.NaN;Double.NaN] 
                   ])
setMatrix app "v" (v.ToArray2D())
app.Execute("res = min(v);") |> ignore
let res = getVector app "res"
let res2 = min v ColumnAxis
