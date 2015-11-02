namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion

type ICovariateStorage =
   abstract member GetSlice : int64 * int64 -> Vector
   abstract member Length : int64

type CovariateStorage(data : Vector) =

    interface ICovariateStorage with
        member this.Length = data.LongLength
        member this.GetSlice(fromObs : int64, toObs : int64) =
           data.View(fromObs, toObs) 


