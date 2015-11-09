namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion
open System.Collections.Generic

type IFactorStorage =
   abstract member GetLevel : int -> string
   abstract member GetSlices : int64 * int64 * int -> seq<UInt16Vector>
   abstract member Length : int64
   abstract member LevelCount : int

type FactorStorage internal (length : int64, nativeArray : Choice<nativeptr<uint8>, nativeptr<uint16>>,
                             levelMap : Dictionary<uint16, string>) =

    let mutable isDisposed = false

    new(data : seq<string>) =
        let mutable arr8 = IntPtr.Zero
        let mutable arr16 = IntPtr.Zero
        let levelMap = Dictionary<uint16, string>()
        let levelIndexMap = Dictionary<string, uint16>()
        let nativeArrayUInt8Ptr = &&arr8 |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt8Ptr>
        let nativeArrayUInt16Ptr = &&arr16 |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt16Ptr>
        let len, bufferSize, nativeArrayUInt8, nativeArrayUInt16, useUInt16 =
            data |> Seq.fold (fun (len, bufferSize, nativeArrayUInt8, nativeArrayUInt16, useUInt16) a ->
                                  let levelIndex, newUseInt16 =
                                      if levelIndexMap.ContainsKey(a) then levelIndexMap.[a], useUInt16
                                      else
                                          let levelIndex' = levelIndexMap.Count |>  Checked.uint16
                                          levelIndexMap.Add(a, levelIndex')
                                          levelMap.Add(levelIndex', a)
                                          levelIndex', (levelIndex' > 255us)

                                  let bufferSize =
                                      if len >= bufferSize then 
                                          let newBufferSize = if len < 1000000L then 1000000L else len + 1000000L
                                          if not newUseInt16 then
                                              MklFunctions.UI8_Resize_Array(newBufferSize, nativeArrayUInt8)
                                          else
                                              MklFunctions.UI16_Resize_Array(newBufferSize, nativeArrayUInt16)
                                          newBufferSize
                                      else bufferSize
                                  if newUseInt16 <> useUInt16 then
                                      if nativeArrayUInt16 |> NativePtr.read |> NativePtr.toNativeInt = IntPtr.Zero then
                                          MklFunctions.UI16_Resize_Array(bufferSize, nativeArrayUInt16)
                                      MklFunctions.UI8_UI16_Convert_Array(len, (NativePtr.read nativeArrayUInt8), (NativePtr.read nativeArrayUInt16))
                                      MklFunctions.Free_Array(NativePtr.read nativeArrayUInt8 |> NativePtr.toNativeInt)
                                  if not newUseInt16 then
                                      MklFunctions.UI8_Set_Item(len, (NativePtr.read nativeArrayUInt8), levelIndex |> uint8)
                                  else
                                      MklFunctions.UI16_Set_Item(len, (NativePtr.read nativeArrayUInt16), levelIndex)
                                  (len + 1L, bufferSize, nativeArrayUInt8, nativeArrayUInt16, newUseInt16)
                             ) (0L, 0L, nativeArrayUInt8Ptr, nativeArrayUInt16Ptr, false)
        if bufferSize > len then 
            if not useUInt16 then
                MklFunctions.UI8_Resize_Array(len, nativeArrayUInt8)
            else
                MklFunctions.UI16_Resize_Array(len, nativeArrayUInt16)
        let nativeArray = if not useUInt16 then Choice1Of2 (NativePtr.read nativeArrayUInt8) else Choice2Of2 (NativePtr.read nativeArrayUInt16)
        new FactorStorage(len, nativeArray, levelMap)

    interface IFactorStorage with
        member __.Length = length |> int64
        member __.GetLevel(levelIndex) = levelMap.[levelIndex |> uint16]
        member __.LevelCount = levelMap.Count
        member __.GetSlices (fromObs : int64, toObs : int64, sliceLength : int) =
            match nativeArray with
                | Choice1Of2(nativeArray) ->
                    seq
                      {
                        let sizeof = sizeof<uint8> |> int64
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        use buffer = new UInt16Vector(sliceLength, 0us)
                        for i in 0..m-1 do
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(i) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<uint8>
                            MklFunctions.UI8_UI16_Convert_Array(sliceLength, offsetAddr, buffer.NativeArray)
                            yield buffer
                        if k > 0L then
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(m) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<uint8>
                            MklFunctions.UI8_UI16_Convert_Array(k, offsetAddr, buffer.NativeArray)
                            yield buffer.View(0L, k-1L)
                      }

                | Choice2Of2(nativeArray) ->
                    seq
                      {
                        let sizeof = sizeof<uint16> |> int64
                        let length = toObs - fromObs + 1L
                        let sliceLength = int64 sliceLength
                        let m = length / sliceLength |> int
                        let k = length % sliceLength 
                        use buffer = new UInt16Vector(sliceLength, 0us)
                        for i in 0..m-1 do
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(i) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<uint16>
                            MklFunctions.UI16_Copy_Array(sliceLength, offsetAddr, buffer.NativeArray)
                            yield buffer
                        if k > 0L then
                            let offsetAddr = IntPtr((nativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(m) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<uint16>
                            MklFunctions.UI16_Copy_Array(k, offsetAddr, buffer.NativeArray)
                            yield buffer.View(0L, k-1L)
                      }

    interface IDisposable with
        member this.Dispose() = this.DoDispose(true)

    member internal this.DoDispose(isDisposing) = if not isDisposed && length > 0L then
                                                     isDisposed <- true
                                                     if isDisposing then GC.SuppressFinalize(this)
                                                     let nativeArray = 
                                                         match nativeArray with
                                                             | Choice1Of2(arr) -> arr |> NativePtr.toNativeInt
                                                             | Choice2Of2(arr) -> arr |> NativePtr.toNativeInt
                                                     if nativeArray <> IntPtr.Zero then MklFunctions.Free_Array(nativeArray)

    override this.Finalize() = try this.DoDispose(false) with _ -> ()


