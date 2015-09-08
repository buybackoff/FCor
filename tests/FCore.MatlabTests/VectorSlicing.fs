namespace FCore.MatlabTests

open FCore
open FCore.Math
open FCore.BasicStats
open FCore.Random
open Xunit
open FsUnit
open FsUnit.Xunit
open FsCheck
open FsCheck.Xunit
open System
open MLApp
open Util

module VectorSlicing =

//    let app = new MLAppClass()
//    do app.Visible <- 0

    let inline (<=>) (x : float[]) (y :float[]) = epsEqualArray x y epsEqualFloat 0.0

    let inline epsEqual eps (x : float[]) (y :float[])  = epsEqualArray x y epsEqualFloat eps

//    [<Property>]
//    let ``GetSlice int64``(v : float[]) (fromIndex : int option) (toIndex : int option) =
//        setVector app "v" v
//        app.Execute("res = sum(v);") |> ignore
//        let res = getScalar app "res"
//        let v = new Vector(v)

