namespace FCor

module GenericFormatting =
    open System
    open System.Collections.Generic

    type GenericFormat<'T>() =
        static member val Value = fun (x : 'T) -> "G" with get, set

    type GenericFormat private() =

//        static do GenericFormat<float32>.Value <-  fun x -> "G4"
//        static do GenericFormat<float>.Value <-  fun x -> "G4"

        static let instance = new GenericFormat()

        static member Instance = instance

        member this.GetFormat<'T>() = GenericFormat<'T>.Value

        member this.SetFormat<'T>(format) = GenericFormat<'T>.Value <- format
         



