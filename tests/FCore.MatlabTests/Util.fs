namespace FCore.MatlabTests
open System
open System.Runtime.InteropServices
open FCore
open MLApp

module Util =

    let fixEmpty (a : 'T[,]) =
        if a.Length = 0 then Array2D.zeroCreate<'T> 0 0 
        else a

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

    let getBoolMatrix (app : MLAppClass) (varName : string) =
        app.Execute(sprintf "is_empty=isempty(%s)" varName ) |> ignore
        let is_empty = app.GetVariable("is_empty", "base"):?>bool
        if is_empty then Array2D.create 0 0 false
        else
            let m = app.GetVariable(varName, "base")
            match m with
                | :? bool as v -> Array2D.create 1 1 v
                | :? Array as v -> v:?>bool[,]
                | :? System.Reflection.Missing -> Array2D.create 0 0 false
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
                                if v.GetLength(0) > 1 && v.GetLength(1) > 1 then raise (new ArgumentException("not vector"))
                                let v = v:?>float[,] |> fixEmpty
                                if v.GetLength(0) > v.GetLength(1) then
                                    Array.init (max (v.GetLength(0)) (v.GetLength(1))) (fun i -> v.[i, 0])
                                else
                                    Array.init (max (v.GetLength(0)) (v.GetLength(1))) (fun i -> v.[0, i])
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
                                if v.GetLength(0) > 1 && v.GetLength(1) > 1 then raise (new ArgumentException("not vector"))
                                let v = v:?>float[,] |> fixEmpty
                                if v.GetLength(0) > v.GetLength(1) then
                                    Array.init (max (v.GetLength(0)) (v.GetLength(1))) (fun i -> v.[i, 0])
                                else
                                    Array.init (max (v.GetLength(0)) (v.GetLength(1))) (fun i -> v.[0, i])
                            | :? System.Reflection.Missing -> Array.create 0 0.0
                            | _ -> raise (InvalidOperationException())
                | :? System.Reflection.Missing -> Array.create 0 0.0
                | _ -> raise (InvalidOperationException())

    let getBoolVector (app : MLAppClass) (varName : string) =
        app.Execute(sprintf "is_empty=isempty(%s)" varName ) |> ignore
        let is_empty = app.GetVariable("is_empty", "base"):?>bool
        if is_empty then Array.create 0 false
        else
            let m = app.GetVariable(varName, "base")
            match m with
                | :? bool as v -> Array.create 1 v
                | :? Array as v ->
                    if v.GetLength(0) > 1 && v.GetLength(1) > 1 then raise (new ArgumentException("not vector"))
                    let v = v:?>bool[,] |> fixEmpty
                    if v.GetLength(0) > v.GetLength(1) then
                        Array.init (max (v.GetLength(0)) (v.GetLength(1))) (fun i -> v.[i, 0])
                    else
                        Array.init (max (v.GetLength(0)) (v.GetLength(1))) (fun i -> v.[0, i])
                | :? System.Reflection.Missing -> Array.create 0 false
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

    let getBoolScalar (app : MLAppClass) (varName : string) =
        Convert.ToBoolean(app.GetVariable(varName, "base"))

    let setMatrix (app : MLAppClass) (varName : string) (v : float[,]) =
        if v.Length = 1 && Double.IsNaN(v.[0, 0]) then
            app.Execute(sprintf "%s = [NaN];" varName) |> ignore
        elif v.Length = 1 && Double.IsNegativeInfinity(v.[0, 0]) then
            app.Execute(sprintf "%s = [-Inf];" varName) |> ignore
        elif v.Length = 1 && Double.IsPositiveInfinity(v.[0, 0]) then
            app.Execute(sprintf "%s = [Inf];" varName) |> ignore
        else
            app.PutWorkspaceData(varName, "base", v)

    let setBoolMatrix (app : MLAppClass) (varName : string) (v : bool[,]) =
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

    let setBoolVector (app : MLAppClass) (varName : string) (v : bool[]) =
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

    let setBoolScalar (app : MLAppClass) (varName : string) (v : bool) =
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


    let inline epsEqualArray2D (a : 'T[,]) (b : 'T[,]) (epsEqual : 'T -> 'T -> 'S -> bool) (eps : 'S) =
        if a.Length = 0 && b.Length = 0 then true
        else
            let s1 = seq{for col in 0..a.GetLength(1)-1 do for row in 0..a.GetLength(0)-1 do yield a.[row,col]}
            let s2 = seq{for col in 0..b.GetLength(1)-1 do for row in 0..b.GetLength(0)-1 do yield b.[row,col]}
            (a.GetLength(0) = b.GetLength(0)) && (a.GetLength(1) = b.GetLength(1)) &&
            (s2 |> Seq.zip s1 |> Seq.map (fun (x,y) -> epsEqual x y eps) |> Seq.fold (&&) true)

    let array2DZip (x : 'T[,]) (y : 'T[,]) =
        if x.GetLength(0) <> y.GetLength(0) || x.GetLength(1) <> y.GetLength(1) then
            raise (new ArgumentException())
        else
            Array2D.init (x.GetLength(0)) (x.GetLength(1)) (fun i j -> x.[i, j], y.[i, j])

    let array2DExists (pred : 'T -> bool) (x : 'T[,])=
        seq{for i in 0..x.GetLength(0)-1 do for j in 0..x.GetLength(1)-1 do yield x.[i, j]} |> Seq.exists pred

    let array2DFilter (pred : 'T -> bool) (x : 'T[,])=
        seq{for i in 0..x.GetLength(0)-1 do for j in 0..x.GetLength(1)-1 do yield x.[i, j]} |> Seq.filter pred |> Seq.toArray


        

