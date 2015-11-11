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

type CovariateStorageFloat32 () =
    let mutable isDisposed = false
    let mutable length = 0L
    let mutable bufferSize = 0L
    let mutable nativeArray : nativeptr<float32> = IntPtr.Zero |> NativePtr.ofNativeInt<float32>

    member this.SetSlice(fromObs : int64, data : float32[]) =
        let newLength = fromObs + int64(data.Length)
        let newBufferSize = if newLength <= bufferSize then bufferSize else bufferSize + 1000000L
        if newBufferSize > bufferSize then
            let mutable natArr = nativeArray
            let nativeArrayPtr = &&natArr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<Float32Ptr> 
            MklFunctions.S_Resize_Array(newBufferSize, nativeArrayPtr) 
            nativeArray <- NativePtr.read nativeArrayPtr
            data |> Array.iteri (fun i x -> MklFunctions.S_Set_Item(fromObs + int64(i), nativeArray, x))
        else
            data |> Array.iteri (fun i x -> MklFunctions.S_Set_Item(fromObs + int64(i), nativeArray, x))
        length <- newLength
        bufferSize <- newBufferSize

    interface ICovariateStorage with
        member this.Length = length
        member __.GetSlices (fromObs : int64, toObs : int64, sliceLength : int) =
                    seq
                      {
                        let sizeof = sizeof<float32> |> int64
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        use buffer = new Vector(sliceLength, 0.0)
                        for i in 0..m-1 do
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(i) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float32>
                            MklFunctions.S_D_Convert_Array(sliceLength, offsetAddr, buffer.NativeArray)
                            yield buffer
                        if k > 0L then
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(m) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float32>
                            MklFunctions.S_D_Convert_Array(k, offsetAddr, buffer.NativeArray)
                            yield buffer.View(0L, k-1L)
                      }

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = if not isDisposed && length > 0L then
                                                     isDisposed <- true
                                                     if isDisposing then GC.SuppressFinalize(this)
                                                     let nativeArray = nativeArray |> NativePtr.toNativeInt
                                                     if nativeArray <> IntPtr.Zero then MklFunctions.Free_Array(nativeArray)

    override this.Finalize() = try this.DoDispose(false) with _ -> ()



