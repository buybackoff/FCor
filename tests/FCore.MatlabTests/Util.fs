namespace FCore.MatlabTests
open System
open System.Runtime.InteropServices
open FCore
open MLApp

module Util =

    let getMatrix (app : MLAppClass) (varName : string) =
        app.Execute(sprintf "is_nan=isnan(%s);is_empty=isempty(%s)" varName varName ) |> ignore
        let is_empty = app.GetVariable("is_empty", "base"):?>bool
        if is_empty then Array2D.create 0 0 0.0
        else
            let is_nan = app.GetVariable("is_nan", "base")
            match is_nan with
                | :? bool as v ->
                    if v then Array2D.create 1 1 Double.NaN
                    else
                        let m = app.GetVariable(varName, "base")
                        match m with
                            | :? float as v -> Array2D.create 1 1 v
                            | :? Array as v -> v:?>float[,]
                            | :? System.Reflection.Missing -> Array2D.create 0 0 0.0
                            | _ -> raise (InvalidOperationException())
                | :? Array as v ->
                    let v = v:?>bool[,]
                    if v = Array2D.create 1 1 true then
                        Array2D.create 1 1 Double.NaN
                    else
                        let m = app.GetVariable(varName, "base")
                        match m with
                            | :? float as v -> Array2D.create 1 1 v
                            | :? Array as v -> v:?>float[,]
                            | :? System.Reflection.Missing -> Array2D.create 0 0 0.0
                            | _ -> raise (InvalidOperationException())
                | :? System.Reflection.Missing -> Array2D.create 0 0 0.0
                | _ -> raise (InvalidOperationException())

    let getVector (app : MLAppClass) (varName : string) =
        app.Execute(sprintf "is_nan=isnan(%s);is_empty=isempty(%s)" varName varName ) |> ignore
        let is_empty = app.GetVariable("is_empty", "base"):?>bool
        if is_empty then Array.create 0 0.0
        else
            let is_nan = app.GetVariable("is_nan", "base")
            match is_nan with
                | :? bool as v ->
                    if v then [|Double.NaN|]
                    else
                        let m = app.GetVariable(varName, "base")
                        match m with
                            | :? float as v -> Array.create 1 v
                            | :? Array as v ->
                                if v.GetLength(1) > 1 then raise (new ArgumentException("not vector"))
                                let v = v:?>float[,]
                                Array.init (v.GetLength(0)) (fun i -> v.[i, 0])
                            | :? System.Reflection.Missing -> Array.create 0 0.0
                            | _ -> raise (InvalidOperationException())
                | :? Array as v ->
                    let v = v:?>bool[,]
                    if v = Array2D.create 1 1 true then
                        [|Double.NaN|]
                    else
                        let m = app.GetVariable(varName, "base")
                        match m with
                            | :? float as v -> Array.create 1 v
                            | :? Array as v ->
                                if v.GetLength(1) > 1 then raise (new ArgumentException("not vector"))
                                let v = v:?>float[,]
                                Array.init (v.GetLength(0)) (fun i -> v.[i, 0])
                            | :? System.Reflection.Missing -> Array.create 0 0.0
                            | _ -> raise (InvalidOperationException())
                | :? System.Reflection.Missing -> Array.create 0 0.0
                | _ -> raise (InvalidOperationException())

    let getScalar (app : MLAppClass) (varName : string) =
        app.Execute(sprintf "is_nan=isnan(%s);" varName ) |> ignore
        let is_nan = app.GetVariable("is_nan", "base")
        match is_nan with
            | :? bool as v ->
                if v then
                    Double.NaN
                else
                    Convert.ToDouble(app.GetVariable(varName, "base"))
            | _ -> raise (InvalidOperationException())

    let setMatrix (app : MLAppClass) (varName : string) (v : float[,]) =
        if v.Length = 1 && Double.IsNaN(v.[0, 0]) then
            app.Execute(sprintf "%s = [NaN];" varName) |> ignore
        elif v.Length = 1 && Double.IsNegativeInfinity(v.[0, 0]) then
            app.Execute(sprintf "%s = [-Inf];" varName) |> ignore
        elif v.Length = 1 && Double.IsPositiveInfinity(v.[0, 0]) then
            app.Execute(sprintf "%s = [Inf];" varName) |> ignore
        else
            app.PutWorkspaceData(varName, "base", v)

    let setVector (app : MLAppClass) (varName : string) (v : float[]) =
        if v.Length = 1 && Double.IsNaN(v.[0]) then
            app.Execute(sprintf "%s = [NaN];" varName) |> ignore
        elif v.Length = 1 && Double.IsNegativeInfinity(v.[0]) then
            app.Execute(sprintf "%s = [-Inf];" varName) |> ignore
        elif v.Length = 1 && Double.IsPositiveInfinity(v.[0]) then
            app.Execute(sprintf "%s = [Inf];" varName) |> ignore
        else
            let v = Array2D.init v.Length 1 (fun i j -> v.[i])
            app.PutWorkspaceData(varName, "base", v)

    let setScalar (app : MLAppClass) (varName : string) (v : float) =
        if Double.IsNaN(v) then
            app.Execute(sprintf "%s = [NaN];" varName) |> ignore
        elif Double.IsNegativeInfinity(v) then
            app.Execute(sprintf "%s = [-Inf];" varName) |> ignore
        elif Double.IsPositiveInfinity(v) then
            app.Execute(sprintf "%s = [Inf];" varName) |> ignore
        else
            app.PutWorkspaceData(varName, "base", v)

    let inline epsEqualFloat x y eps =
        if x = y then true
        else
            if Double.IsNaN(x) && Double.IsNaN(y) then true
            elif Double.IsNegativeInfinity(x) && Double.IsNegativeInfinity(y) then true
            elif Double.IsPositiveInfinity(x) && Double.IsPositiveInfinity(y) then true
            elif abs(x) <= eps && abs(y) <= eps then true
            else abs(x-y)/(max (abs(x)) (abs(y))) <= eps

    let inline epsEqualArray (a : 'T[]) (b : 'T[]) (epsEqual : 'T -> 'T -> 'S -> bool) (eps : 'S) =
        if a.Length = 0 && b.Length = 0 then true
        else (a.Length = b.Length) && (b |> Array.zip a |> Array.map (fun (x,y) -> epsEqual x y eps) |> Array.fold (&&) true)



        

