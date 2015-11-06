namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion

type ICovariateStorage =
   abstract member GetSlices : int64 * int64 * int -> seq<Vector>
   abstract member Length : int64

type CovariateStorage(data : Vector) =

    interface ICovariateStorage with
        member this.Length = data.LongLength
        member __.GetSlices (fromObs : int64, toObs : int64, sliceLength : int) =
            seq
              {
                let sizeof = sizeof<float> |> int64
                let length = toObs - fromObs + 1L
                let sliceLength = int64 sliceLength
                let m = length / sliceLength |> int
                let k = length % sliceLength 
                use buffer = new Vector(sliceLength, 0.0)
                for i in 0..m-1 do
                    let offsetAddr = IntPtr((data.NativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(i) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float>
                    MklFunctions.D_Copy_Array(sliceLength, offsetAddr, buffer.NativeArray)
                    yield buffer
                if k > 0L then
                    let offsetAddr = IntPtr((data.NativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(m) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float>
                    MklFunctions.D_Copy_Array(k, offsetAddr, buffer.NativeArray)
                    yield buffer.View(0L, k-1L)
              }


