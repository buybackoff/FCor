#r @".\bin\release\FCore.dll"
open FCore
open FCore.ExplicitConversion
open FCore.Math
open FCore.LinearAlgebra
open System
open System.IO
open System.Runtime.InteropServices
open System.Collections.Generic
open FCore.Random

open Overloading
open BasicStats

let m = 10
let n = 5
let rng = new MT19937Rng()
let A = rand rng m n
let eye : Matrix = I m m
let l,u,p = lu A
let v = eye.[p, {0..m-1}]
let res = (v * l) * u



