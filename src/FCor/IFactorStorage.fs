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
   abstract member GetSlice : int64 * int64 -> IntVector
   abstract member Length : int64
   abstract member LevelCount : int

type FactorStorage(length : int) =
    let levelMap = new Dictionary<int, string>()
    let levelIndexMap = new Dictionary<string, int>()
    let data = Array.zeroCreate<byte> length

    interface IFactorStorage with
        member __.Length = length |> int64
        member __.GetLevel(levelIndex) = levelMap.[levelIndex]
        member __.LevelCount = levelMap.Count
        member __.GetSlice(fromObs : int64, toObs : int64) =
            let count = (toObs - fromObs + 1L) |> int
            let slice = Array.sub data (int(fromObs)) count |> Array.map int
            new IntVector(slice, false)

    member this.SetSlice(fromObs : int, sliceData : string[]) =
        sliceData |> Array.iteri (fun i x -> 
                                      if levelIndexMap.ContainsKey(x) then data.[fromObs + i] <- byte(levelIndexMap.[x])
                                      else
                                          let index = levelIndexMap.Count
                                          data.[fromObs + i] <- index |> byte
                                          levelMap.Add(index, x)
                                          levelIndexMap.Add(x, index)
                                 )

