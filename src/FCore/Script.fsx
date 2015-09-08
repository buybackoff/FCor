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

let res = var(new Vector([1.0;1e150]))



