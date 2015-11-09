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

type CovariateStorage internal (length : int64, nativeArray : Choice<nativeptr<float32>, nativeptr<float>, Vector>) =
    let mutable isDisposed = false

    new(data : Vector) =
        new CovariateStorage(data.LongLength, Choice3Of3 data)

    new(data : seq<float32>) =
        let mutable arr = IntPtr.Zero
        let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<Float32Ptr>
        let len, bufferSize, nativeArray =
            data |> Seq.fold (fun (len, bufferSize, nativeArray) a ->
                                  let bufferSize =
                                      if len >= bufferSize then 
                                          let newBufferSize = if len < 1000000L then 1000000L else len + 1000000L
                                          MklFunctions.S_Resize_Array(newBufferSize, nativeArray)
                                          newBufferSize
                                      else bufferSize
                                  MklFunctions.S_Set_Item(len, (NativePtr.read nativeArray), a)
                                  (len + 1L, bufferSize, nativeArray)
                             ) (0L, 0L, nativeArrayPtr)
        if bufferSize > len then 
            MklFunctions.S_Resize_Array(len, nativeArray)
        new CovariateStorage(len,Choice1Of3 (NativePtr.read nativeArray))

    new(data : seq<float>) =
        let mutable arr = IntPtr.Zero
        let nativeArrayPtr = &&arr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<FloatPtr>
        let len, bufferSize, nativeArray =
            data |> Seq.fold (fun (len, bufferSize, nativeArray) a ->
                                  let bufferSize =
                                      if len >= bufferSize then 
                                          let newBufferSize = if len < 1000000L then 1000000L else len + 1000000L
                                          MklFunctions.D_Resize_Array(newBufferSize, nativeArray)
                                          newBufferSize
                                      else bufferSize
                                  MklFunctions.D_Set_Item(len, (NativePtr.read nativeArray), a)
                                  (len + 1L, bufferSize, nativeArray)
                             ) (0L, 0L, nativeArrayPtr)
        if bufferSize > len then 
            MklFunctions.D_Resize_Array(len, nativeArray)
        new CovariateStorage(len,Choice2Of3 (NativePtr.read nativeArray))

    interface ICovariateStorage with
        member this.Length = length
        member __.GetSlices (fromObs : int64, toObs : int64, sliceLength : int) =
            match nativeArray with
                | Choice1Of3(nativeArray) ->
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

                | Choice2Of3(nativeArray) ->
                    seq
                      {
                        let sizeof = sizeof<float> |> int64
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        use buffer = new Vector(sliceLength, 0.0)
                        for i in 0..m-1 do
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(i) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float>
                            MklFunctions.D_Copy_Array(sliceLength, offsetAddr, buffer.NativeArray)
                            yield buffer
                        if k > 0L then
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(m) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float>
                            MklFunctions.D_Copy_Array(k, offsetAddr, buffer.NativeArray)
                            yield buffer.View(0L, k-1L)
                      }

                | Choice3Of3(vector) ->
                    let nativeArray = vector.NativeArray
                    seq
                      {
                        let sizeof = sizeof<float> |> int64
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        use buffer = new Vector(sliceLength, 0.0)
                        for i in 0..m-1 do
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(i) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float>
                            MklFunctions.D_Copy_Array(sliceLength, offsetAddr, buffer.NativeArray)
                            yield buffer
                        if k > 0L then
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(m) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<float>
                            MklFunctions.D_Copy_Array(k, offsetAddr, buffer.NativeArray)
                            yield buffer.View(0L, k-1L)
                      }

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = if not isDisposed && length > 0L then
                                                     isDisposed <- true
                                                     if isDisposing then GC.SuppressFinalize(this)
                                                     let nativeArray = 
                                                         match nativeArray with
                                                             | Choice1Of3(arr) -> arr |> NativePtr.toNativeInt
                                                             | Choice2Of3(arr) -> arr |> NativePtr.toNativeInt
                                                             | Choice3Of3(_) -> IntPtr.Zero 
                                                     if nativeArray <> IntPtr.Zero then MklFunctions.Free_Array(nativeArray)

    override this.Finalize() = try this.DoDispose(false) with _ -> ()



