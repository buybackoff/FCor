namespace FCor
#nowarn "9"

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open System.Collections.Generic
open FCor.ExplicitConversion
open System.Text

type DataFrame(statVars : StatVariable list) =

    static let empty = new DataFrame([])

    static member Empty = empty

    member this.Factors = statVars |> List.choose (fun svar -> match svar with | StatVariable.Factor(f) -> Some f | _ -> None)

    member this.Covariates = statVars |> List.choose (fun svar -> match svar with | StatVariable.Covariate(c) -> Some c | _ -> None)

    member this.Item
        with get(name : string) = statVars |> List.find (fun svar -> svar.Name = name)

    interface IFormattable with
        member this.ToString(_, _) = 
            let sb = new StringBuilder()
            this.Factors |> List.iter (fun f -> sb.AppendLine(f.ToString()) |> ignore)
            this.Covariates |> List.iter (fun c -> sb.AppendLine(c.ToString()) |> ignore)
            sb.ToString()

    override this.ToString() =
        (this:>IFormattable).ToString("", null)



        