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
let app = new MLAppClass()

#time
let rng = new MT19937Rng()
let rnd = new Random()

let v = Array2D.create 2 3 1.1
setMatrix app "v" v
let indices = [|0..v.Length-1|] |> Array.map (fun x -> rnd.Next(v.Length))
setVector app "indices" (indices |> Array.map (fun index -> float(index + 1)))
app.Execute("res = v(indices);") |> ignore
let res = getVector app "res"
let V = new Matrix(v)
let aa = epsEqualArray (V.[indices |> Array.toSeq].ToArray()) res epsEqualFloat 0.0      
