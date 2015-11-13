namespace FCor
#nowarn "9"

open System
open System.Text
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic

type DisplayControl() =
    static let mutable maxDisplaySize = (10L, 10L)

    static member MaxDisplaySize
        with get() = maxDisplaySize
        and set(size) = 
            maxDisplaySize <- size

    static member FormatArray2D(array2D : 'T[,], format : string, moreRows : bool, moreCols : bool) =
        let more = "..."
        let sb = new StringBuilder()
        let mutable maxChars = 3
        for i in 0..array2D.GetLength(0)-1 do
            for j in 0..array2D.GetLength(1)-1 do
                let v = array2D.[i, j]
                let len = String.Format("{0:" + format + "}", v).Length
                if (len > maxChars) then maxChars <- len
        maxChars <- maxChars + 1
        for i in 0..array2D.GetLength(0)-1 do
            for j in 0..array2D.GetLength(1)-1 do
                let v = array2D.[i, j]
                let formattedVal = String.Format("{0:" + format + "}", v).PadLeft(maxChars)
                sb.Append(formattedVal) |> ignore
            if moreCols then sb.Append(more.PadLeft(maxChars)) |> ignore
            sb.Append("\r\n") |> ignore
        if moreRows then 
            for j in 0..array2D.GetLength(1)-1 do
                sb.Append(more.PadLeft(maxChars)) |> ignore
        if moreCols then sb.Append(more.PadLeft(maxChars)) |> ignore
        sb.Append("\r\n") |> ignore
        sb.ToString()

