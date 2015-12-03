namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion
open System.Collections.Generic

type IFactorStorage =
   abstract member Level : int -> string with get
   abstract member GetSlices : int64 * int64 * int -> seq<UInt16Vector>
   abstract member Length : int64
   abstract member Cardinality : int

type FactorStorage () =
    let levelMap = Dictionary<uint16, string>()
    let levelIndexMap = Dictionary<string, uint16>()
    let mutable isDisposed = false
    let mutable length = 0L
    let mutable bufferSize = 0L
    let mutable nativeArray : Choice<nativeptr<uint8>, nativeptr<uint16>> = IntPtr.Zero |> NativePtr.ofNativeInt<uint8> |> Choice1Of2

    member this.SetSlice(fromObs : int64, levels : string[]) =
        levels |> Array.iter (fun level -> let level = level.Trim()
                                           if not <| levelIndexMap.ContainsKey(level) then
                                               let levelIndex' = levelIndexMap.Count |>  Checked.uint16
                                               levelIndexMap.Add(level, levelIndex')
                                               levelMap.Add(levelIndex', level))

        let newLength = fromObs + int64(levels.Length)
        let newBufferSize = if newLength <= bufferSize then bufferSize else bufferSize + 1000000L
        if levelIndexMap.Count > 255 then
            match nativeArray with
                | Choice1Of2(natArr) -> 
                    let ui8Allocated = natArr |> NativePtr.toNativeInt <> IntPtr.Zero
                    let mutable arr16 = IntPtr.Zero
                    let nativeArrayUInt16Ptr = &&arr16 |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt16Ptr>
                    MklFunctions.UI16_Resize_Array(newLength, nativeArrayUInt16Ptr)
                    let nativeArrayUInt16 = NativePtr.read nativeArrayUInt16Ptr
                    if ui8Allocated then
                        MklFunctions.UI8_UI16_Convert_Array(length, natArr, nativeArrayUInt16)
                        MklFunctions.Free_Array(natArr |> NativePtr.toNativeInt)
                    levels |> Array.iteri (fun i level -> 
                                               let level = level.Trim()
                                               let levelIndex = levelIndexMap.[level]
                                               MklFunctions.UI16_Set_Item(fromObs + int64(i), nativeArrayUInt16, levelIndex)
                                          )
                    nativeArray <- Choice2Of2(nativeArrayUInt16)
                | Choice2Of2(natArr) ->
                    let mutable natArr = natArr
                    let nativeArrayUInt16Ptr = &&natArr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt16Ptr>
                    MklFunctions.UI16_Resize_Array(newLength, nativeArrayUInt16Ptr)
                    let nativeArrayUInt16 = NativePtr.read nativeArrayUInt16Ptr
                    levels |> Array.iteri (fun i level -> 
                                               let level = level.Trim()
                                               let levelIndex = levelIndexMap.[level]
                                               MklFunctions.UI16_Set_Item(fromObs + int64(i), nativeArrayUInt16, levelIndex)
                                          )
                    nativeArray <- Choice2Of2(nativeArrayUInt16)
        else
            match nativeArray with
                | Choice1Of2(natArr) -> 
                    if bufferSize < newBufferSize then
                        let mutable natArr = natArr
                        let nativeArrayUInt8Ptr = &&natArr |> NativePtr.toNativeInt |> NativePtr.ofNativeInt<UInt8Ptr>
                        MklFunctions.UI8_Resize_Array(newBufferSize, nativeArrayUInt8Ptr)
                        let nativeArrayUInt8 = NativePtr.read nativeArrayUInt8Ptr
                        levels |> Array.iteri (fun i level -> 
                                                   let level = level.Trim()
                                                   let levelIndex = levelIndexMap.[level]
                                                   MklFunctions.UI8_Set_Item(fromObs + int64(i), nativeArrayUInt8, levelIndex |> uint8)
                                              )
                        nativeArray <- Choice1Of2(nativeArrayUInt8)
                    else
                        levels |> Array.iteri (fun i level -> 
                                                   let level = level.Trim()
                                                   let levelIndex = levelIndexMap.[level]
                                                   MklFunctions.UI8_Set_Item(fromObs + int64(i), natArr, levelIndex |> uint8)
                                              )
                | _ -> ()
        length <- newLength
        bufferSize <- newBufferSize

    interface IFactorStorage with
        member __.Length = length |> int64
        member __.Level with get(levelIndex) = levelMap.[levelIndex |> uint16]
        member __.Cardinality = levelMap.Count
        member __.GetSlices (fromObs : int64, toObs : int64, sliceLength : int) =
            if fromObs < 0L then raise (new IndexOutOfRangeException())
            if toObs >= length then raise (new IndexOutOfRangeException())
            if sliceLength <= 0 then raise (new ArgumentException("Slice length must be > 0"))
            if fromObs > toObs then Seq.empty
            else
                match nativeArray with
                    | Choice1Of2(nativeArray) ->
                        seq
                          {
                            let sizeof = sizeof<uint8> |> int64
                            let length = toObs - fromObs + 1L
                            let sliceLength = int64 sliceLength
                            let m = length / sliceLength |> int
                            let k = length % sliceLength 
                            use buffer = new UInt16Vector((if m > 0 then sliceLength else k), 0us)
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


