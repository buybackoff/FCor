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
let app = new MLAppClass()
app.Execute("a = rand(100,1000);
             [q, r] = qr(a, 0);") |> ignore
let x = new Matrix(getMatrix app  "a")
let y = new Matrix(getMatrix app  "q")
let z = new Matrix(getMatrix app  "r")
let (q, r) = qr x
epsEqualArray2D (q.ToArray2D()) (y.ToArray2D()) epsEqualFloat 1e-10