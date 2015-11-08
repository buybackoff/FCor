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

type FactorStorage(length : int) =
    let levelMap = new Dictionary<uint16, string>()
    let levelIndexMap = new Dictionary<string, uint16>()
    let data = new UInt16Vector(length, 0us) 

    interface IFactorStorage with
        member __.Length = length |> int64
        member __.GetLevel(levelIndex) = levelMap.[levelIndex |> uint16]
        member __.LevelCount = levelMap.Count
        member __.GetSlices (fromObs : int64, toObs : int64, sliceLength : int) =
            seq
              {
                let sizeof = sizeof<uint16> |> int64
                let length = toObs - fromObs + 1L
                let sliceLength = int64 sliceLength
                let m = length / sliceLength |> int
                let k = length % sliceLength 
                use buffer = new UInt16Vector(sliceLength, 0us)
                for i in 0..m-1 do
                    let offsetAddr = IntPtr((data.NativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(i) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<uint16>
                    MklFunctions.UI16_Copy_Array(sliceLength, offsetAddr, buffer.NativeArray)
                    yield buffer
                if k > 0L then
                    let offsetAddr = IntPtr((data.NativeArray |> NativePtr.toNativeInt).ToInt64() + (fromObs + int64(m) * sliceLength)*sizeof) |> NativePtr.ofNativeInt<uint16>
                    MklFunctions.UI16_Copy_Array(k, offsetAddr, buffer.NativeArray)
                    yield buffer.View(0L, k-1L)
              }

    member this.SetSlice(fromObs : int, sliceData : string[]) =
        sliceData |> Array.iteri (fun i x -> 
                                      if levelIndexMap.ContainsKey(x) then data.[fromObs + i] <- (levelIndexMap.[x])
                                      else
                                          let index = levelIndexMap.Count |> uint16
                                          data.[fromObs + i] <- index 
                                          levelMap.Add(index, x)
                                          levelIndexMap.Add(x, index)
                                 )

